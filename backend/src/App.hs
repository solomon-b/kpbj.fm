{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

{-
TODO:
- Rate Limiting
- Caching
- Test suite
-}

--------------------------------------------------------------------------------

import API
import Auth (checkAuth)
import Cfg.Env (getEnvConfig)
import Config
import Control.Exception (catch)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Reader qualified as Reader
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson ((.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (bimap)
import Data.CaseInsensitive qualified as CI
import Data.Data (Proxy (..))
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Has qualified as Has
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text.Encoding qualified as Text.Encoding
import Database.Class
import Hasql.Connection qualified as HSQL
import Hasql.Pool qualified as HSQL (Pool)
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Pool.Config as HSQL.Pool.Config
import Hasql.Session qualified as HSQL
import Hasql.Statement qualified as HSQL
import Log (runLogT)
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import Network.HTTP.Types.Status qualified as Status
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
import OpenTelemetry.Trace qualified as OTEL
import Servant (Context ((:.)))
import Servant qualified
import Servant.Auth.Server qualified as Auth.Server
import Servant.Auth.Server qualified as Servant.Auth
import System.Posix.Signals qualified as Posix
import Tracing (withTracer)
import Utils

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

        let hsqlSettings = HSQL.settings (fold postgresConfigHost) (fromMaybe 0 postgresConfigPort) (fold postgresConfigUser) (fold postgresConfigPassword) (fold postgresConfigDB)
        let poolSettings = HSQL.Pool.Config.settings [HSQL.Pool.Config.staticConnectionSettings hsqlSettings]
        pgPool <- HSQL.Pool.acquire poolSettings

        jwk <- readJSON "./backend/jwk.json"
        let jwtCfg = Auth.Server.defaultJWTSettings jwk
            cfg = checkAuth pgPool stdOutLogger :. Auth.Server.defaultCookieSettings :. jwtCfg :. Servant.EmptyContext
        withTracer $ \tracerProvider mkTracer -> do
          let tracer = mkTracer OTEL.tracerOptions
          let otelMiddleware = newOpenTelemetryWaiMiddleware' tracerProvider
          Warp.runSettings (warpSettings stdOutLogger appConfigWarpSettings) (otelMiddleware $ app cfg (stdOutLogger, pgPool, jwtCfg, tracer))

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

type AppContext = (Log.Logger, HSQL.Pool, Servant.Auth.JWTSettings, OTEL.Tracer)

type ServantContext = '[Servant.Auth.BasicAuthCfg, Auth.Server.CookieSettings, Auth.Server.JWTSettings]

newtype AppM a = AppM {runAppM' :: AppContext -> Log.LoggerEnv -> IO a}
  deriving
    (Functor, Applicative, Monad, MonadReader AppContext, MonadIO, MonadThrow, MonadCatch, MonadUnliftIO, Log.MonadLog)
    via ReaderT AppContext (Log.LogT IO)

instance MonadDB AppM where
  runDB :: HSQL.Session a -> AppM (Either HSQL.Pool.UsageError a)
  runDB s = do
    pool <- Reader.asks Has.getter
    liftIO $ HSQL.Pool.use pool s

  execStatement :: HSQL.Statement () a -> AppM (Either HSQL.Pool.UsageError a)
  execStatement = runDB . HSQL.statement ()

interpret :: AppContext -> AppM x -> Servant.Handler x
interpret ctx@(logger, _, _, _) (AppM appM) =
  Servant.Handler $ ExceptT $ catch (Right <$> appM ctx (Log.LoggerEnv logger "kpbj-backend" [] [] Log.defaultLogLevel)) $ \(e :: Servant.ServerError) -> pure $ Left e

app :: Servant.Context ServantContext -> AppContext -> Servant.Application
app cfg ctx =
  Servant.serveWithContext (Proxy @API) cfg $
    Servant.hoistServerWithContext
      (Proxy @API)
      (Proxy @ServantContext)
      (interpret ctx)
      server
