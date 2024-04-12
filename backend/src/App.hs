{-# LANGUAGE DerivingVia #-}
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

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (catch)
import Control.Monad (void)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (..))
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
import Effects.MailingList qualified as MailingList
import Handlers.MailingList (MailingListAPI, mailingListHandler)
import Handlers.SplashPage (SplashPageAPI, splashPageHandler)
import Log (runLogT)
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

newtype AppM a = AppM {runAppM :: AppContext -> Log.LoggerEnv -> IO (Either Servant.ServerError a)}
  deriving (Functor, Applicative, Monad, MonadReader AppContext, MonadError Servant.ServerError, MonadIO, Log.MonadLog)
    via ReaderT AppContext (Log.LogT (ExceptT Servant.ServerError IO))

interpret :: AppContext -> AppM x -> Servant.Handler x
interpret ctx@(logger, _) (AppM app) =
  Servant.Handler $ ExceptT $ app ctx (Log.LoggerEnv logger "main" [] [] Log.defaultLogLevel)

app :: Servant.Context ServantContext -> AppContext -> Servant.Application
app cfg ctx =
  Servant.serveWithContext (Proxy @API) cfg $
    Servant.hoistServerWithContext
      (Proxy @API)
      (Proxy @ServantContext)
      (interpret ctx)
      server

--------------------------------------------------------------------------------

type API = SplashPageAPI :<|> MailingListAPI

server ::
  ( MonadError Servant.ServerError m,
    MonadReader env m,
    Has (MVar MailingList.Table) env,
    Log.MonadLog m,
    MonadIO m
  ) =>
  Servant.ServerT API m
server = splashPageHandler :<|> mailingListHandler
