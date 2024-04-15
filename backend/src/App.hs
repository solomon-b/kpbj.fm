{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

{-
TODO:
- JWTs
- Rate Limiting
- Caching
- Tracing
- Test suite
-}

--------------------------------------------------------------------------------

import Cfg qualified
import Cfg.Env (getEnvConfig)
import Config
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
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text.Encoding qualified as Text.Encoding
import Effects.MailingList qualified as MailingList
import Handlers.MailingList (MailingListAPI, mailingListHandler)
import Handlers.SplashPage (SplashPageAPI, splashPageHandler)
import Hasql.Connection qualified as HSQL
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

--------------------------------------------------------------------------------

runApp :: IO ()
runApp =
  Log.withJsonStdOutLogger $ \stdOutLogger -> do
    getEnvConfig @AppConfig >>= \case
      Left err ->
        Log.runLogT "kpbj-backend" stdOutLogger Log.defaultLogLevel $ Log.logAttention "Config Failure" (show err)
      Right AppConfig {..} -> do
        let PostgresConfig {..} = appConfigPostgresSettings
        -- TODO: Is it weird to be instantiating 'LogT' multiple times?
        Log.runLogT "kpbj-backend" stdOutLogger Log.defaultLogLevel $
          Log.logInfo "Launching Service" (KeyMap.fromList ["port" .= warpConfigPort appConfigWarpSettings, "environment" .= appConfigEnvironment])
        let connectionSettings = HSQL.settings postgresConfigHost postgresConfigPort postgresConfigUser postgresConfigPassword (fromMaybe "" postgresConfigDB)
        -- TODO: ERROR NICELY:
        Right connection <- HSQL.acquire connectionSettings
        let jwtCfg = Auth.Server.defaultJWTSettings (error "TODO: CREATE A JWK")
            cfg = Auth.Server.defaultCookieSettings :. jwtCfg :. Servant.EmptyContext
        Warp.runSettings (warpSettings stdOutLogger appConfigWarpSettings) (app cfg (stdOutLogger, connection))

warpSettings :: Log.Logger -> WarpConfig -> Warp.Settings
warpSettings logger' WarpConfig {..} =
  Warp.defaultSettings
    & Warp.setServerName warpConfigServerName
    & Warp.setLogger (warpStructuredLogger logger')
    & Warp.setPort warpConfigPort
    & Warp.setGracefulShutdownTimeout (Just warpConfigTimeout)
    & Warp.setInstallShutdownHandler shutdownHandler

warpStructuredLogger :: Log.Logger -> Wai.Request -> Status.Status -> Maybe Integer -> IO ()
warpStructuredLogger logger' req s sz = do
  reqBody <- Wai.getRequestBodyChunk req
  Log.runLogT "kpbj-backend" logger' Log.defaultLogLevel $
    Log.logInfo "Request" $
      KeyMap.fromList $
        [ "statusCode" .= Status.statusCode s,
          "method" .= Text.Encoding.decodeUtf8 (Wai.requestMethod req),
          "httpVersion" .= show (Wai.httpVersion req),
          "path" .= Text.Encoding.decodeUtf8 (Wai.rawPathInfo req),
          "queryString" .= fmap (bimap Text.Encoding.decodeUtf8 (fmap Text.Encoding.decodeUtf8)) (Wai.queryString req),
          "headers" .= fmap (bimap (Text.Encoding.decodeUtf8 . CI.foldedCase) Text.Encoding.decodeUtf8) (Wai.requestHeaders req),
          "isSecure" .= Wai.isSecure req,
          "remoteHost" .= show (Wai.remoteHost req),
          "requestBody" .= Text.Encoding.decodeUtf8 reqBody
        ]
          <> catMaybes [fmap ("requestSize" .=) sz]

shutdownHandler :: IO () -> IO ()
shutdownHandler closeSocket =
  void $ Posix.installHandler Posix.sigTERM (Posix.CatchOnce closeSocket) Nothing

--------------------------------------------------------------------------------

type AppContext = (Log.Logger, HSQL.Connection)

type ServantContext = '[Auth.Server.CookieSettings, Auth.Server.JWTSettings]

newtype AppM a = AppM {runAppM :: AppContext -> Log.LoggerEnv -> IO (Either Servant.ServerError a)}
  deriving
    (Functor, Applicative, Monad, MonadReader AppContext, MonadError Servant.ServerError, MonadIO, Log.MonadLog)
    via ReaderT AppContext (Log.LogT (ExceptT Servant.ServerError IO))

interpret :: AppContext -> AppM x -> Servant.Handler x
interpret ctx@(logger, _) (AppM app) =
  Servant.Handler $ ExceptT $ app ctx (Log.LoggerEnv logger "kpbj-backend" [] [] Log.defaultLogLevel)

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
    Has HSQL.Connection env,
    Log.MonadLog m,
    MonadIO m
  ) =>
  Servant.ServerT API m
server = splashPageHandler :<|> mailingListHandler
