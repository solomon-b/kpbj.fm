{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

{-
TODO:
- JWTs
- Rate Limiting
- Caching
- Postgres
- Tracing
- Test suite
-}

--------------------------------------------------------------------------------

import Control.Arrow ((>>>))
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Control.Monad (void)
import Control.Monad.Freer qualified as Freer
import Control.Monad.Freer.Exception (Exc)
import Control.Monad.Freer.Exception qualified as Exc
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.Reader qualified as Reader
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson ((.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (bimap)
import Data.CaseInsensitive qualified as CI
import Data.Data (Proxy (..))
import Data.Function ((&))
import Data.Has (Has)
import Data.Has qualified as Has
import Data.Maybe (catMaybes)
import Data.Text.Encoding qualified as Text.Encoding
import Effects.Logger (Logger)
import Effects.Logger qualified as Logger
import Effects.MailingList qualified as MailingList
import Handlers.MailingList (MailingListAPI, mailingListHandler)
import Handlers.SplashPage (SplashPageAPI, splashPageHandler)
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import Network.HTTP.Types.Status qualified as Status
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Context ((:.)), (:<|>) (..), (:>))
import Servant qualified
import Servant.Auth.Server qualified as Auth.Server
import System.Posix.Signals qualified as Posix
import Utils qualified

--------------------------------------------------------------------------------

runApp :: IO ()
runApp =
  Log.withJsonStdOutLogger $ \stdOutLogger -> do
    putStrLn "Launching service on port 3000"
    mvar <- MVar.newMVar (3, MailingList.emailTable)
    let jwtCfg = Auth.Server.defaultJWTSettings (error "TODO: CREATE A JWK")
        cfg = Auth.Server.defaultCookieSettings :. jwtCfg :. Servant.EmptyContext
    Warp.runSettings (warpSettings stdOutLogger) (app cfg (stdOutLogger, mvar))

warpSettings :: Log.Logger -> Warp.Settings
warpSettings logger' =
  Warp.defaultSettings
    & Warp.setLogger (warpStructuredLogger logger')
    & Warp.setPort 3000
    & Warp.setGracefulShutdownTimeout (Just 100)
    & Warp.setInstallShutdownHandler shutdownHandler

warpStructuredLogger :: Log.Logger -> Wai.Request -> Status.Status -> Maybe Integer -> IO ()
warpStructuredLogger logger' req s sz =
  Log.runLogT "main" logger' Log.defaultLogLevel $
    Log.logInfo "Request" $
      KeyMap.fromList $
        [ "statusCode" .= Status.statusCode s,
          "method" .= Text.Encoding.decodeUtf8 (Wai.requestMethod req),
          "httpVersion" .= show (Wai.httpVersion req),
          "path" .= Text.Encoding.decodeUtf8 (Wai.rawPathInfo req),
          "queryString" .= fmap (bimap Text.Encoding.decodeUtf8 (fmap Text.Encoding.decodeUtf8)) (Wai.queryString req),
          "headers" .= fmap (bimap (Text.Encoding.decodeUtf8 . CI.foldedCase) Text.Encoding.decodeUtf8) (Wai.requestHeaders req),
          "isSecure" .= Wai.isSecure req,
          "remoteHost" .= show (Wai.remoteHost req)
          -- "requestBody" .= _ req
        ]
          <> catMaybes [fmap ("requestSize" .=) sz]

shutdownHandler :: IO () -> IO ()
shutdownHandler closeSocket =
  void $ Posix.installHandler Posix.sigTERM (Posix.CatchOnce closeSocket) Nothing

--------------------------------------------------------------------------------

type AppContext = (Log.Logger, MVar MailingList.Table)

type ServantContext = '[Auth.Server.CookieSettings, Auth.Server.JWTSettings]

app :: Servant.Context ServantContext -> AppContext -> Servant.Application
app cfg ctx =
  let stdOutLogger = Has.getter ctx
   in Servant.serveWithContext (Proxy @API) cfg $
        Servant.hoistServerWithContext
          (Proxy @API)
          (Proxy @ServantContext)
          (interpreter stdOutLogger ctx)
          server

interpreter ::
  forall env a.
  ( Has (MVar MailingList.Table) env,
    Has Log.Logger env
  ) =>
  Log.Logger ->
  env ->
  Freer.Eff '[MailingList.Model, Exc MailingList.ModelError, Logger, Exc Servant.ServerError, Reader env, Log.LogT IO] a ->
  Servant.Handler a
interpreter stdOutLogger ctx =
  Servant.Handler
    . ExceptT
    . Log.runLogT "main" stdOutLogger Log.defaultLogLevel
    . Freer.runM
    . flip Reader.runReader ctx
    . Exc.runError
    . Logger.runLogger
    . (MailingList.runMailingListModel @env >>> Utils.toServerError)

--------------------------------------------------------------------------------

type API = SplashPageAPI :<|> MailingListAPI

server ::
  ( Freer.Member Logger r,
    Freer.Member MailingList.Model r,
    Freer.Member (Exc Servant.ServerError) r
  ) =>
  Servant.ServerT API (Freer.Eff r)
server = splashPageHandler :<|> mailingListHandler
