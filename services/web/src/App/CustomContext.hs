{-# OPTIONS_GHC -Wno-orphans #-}

module App.CustomContext
  ( -- * Custom Context
    CustomContext (..),

    -- * Two-phase initialization
    CustomConfigs,
    loadCustomConfigs,
    buildCustomContext,

    -- * Re-exports
    StorageContext (..),
    StorageBackend (..),
    AnalyticsConfig (..),
    GoogleAnalyticsId (..),
    SmtpConfig (..),
    PlayoutSecret (..),
    WebhookSecret (..),
    CleanupInterval (..),
    WebhookConfig (..),
  )
where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import App.Analytics (AnalyticsConfig (..), GoogleAnalyticsId (..), loadAnalyticsConfig)
import App.Context (AppContext (..))
import App.Smtp (SmtpConfig (..), loadSmtpConfig)
import App.Storage (S3ConfigResult (..), StorageBackend (..), StorageContext (..), loadStorageConfig, withStorageContext)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has qualified as Has
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Effects.BackgroundJobs qualified as BackgroundJobs
import Log qualified
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

-- | Secret used to authenticate Liquidsoap playout requests.
newtype PlayoutSecret = PlayoutSecret {unPlayoutSecret :: Maybe Text}
  deriving newtype (Eq)

instance Show PlayoutSecret where
  show _ = "**********"

newtype WebhookSecret = WebhookSecret {unWebhookSecret :: Text}
  deriving newtype (Eq)

instance Show WebhookSecret where
  show _ = "**********"

-- | Interval in seconds between background cleanup job runs.
newtype CleanupInterval = CleanupInterval {unCleanupInterval :: Int}
  deriving stock (Show, Eq)

-- | Configuration for webhook service used to restart containers.
data WebhookConfig
  = -- | Webhook is not configured (missing URL or secret).
    WebhookDisabled
  | -- | Webhook is fully configured with base URL and secret.
    WebhookEnabled
      { wcBaseUrl :: Text,
        wcSecret :: WebhookSecret
      }
  deriving stock (Show, Eq)

-- | Custom application context containing all subsystem configurations.
--
-- This is the type passed to 'App.runApp' and made available to handlers
-- via the 'Has' typeclass.
data CustomContext = CustomContext
  { storageContext :: StorageContext,
    analyticsConfig :: AnalyticsConfig,
    smtpConfig :: Maybe SmtpConfig,
    playoutSecret :: PlayoutSecret,
    cleanupInterval :: CleanupInterval,
    webhookConfig :: WebhookConfig
  }

--------------------------------------------------------------------------------
-- Has instances for CustomContext

instance Has.Has StorageContext CustomContext where
  getter = storageContext
  modifier f ctx = ctx {storageContext = f (storageContext ctx)}

instance Has.Has AnalyticsConfig CustomContext where
  getter = analyticsConfig
  modifier f ctx = ctx {analyticsConfig = f (analyticsConfig ctx)}

instance Has.Has (Maybe SmtpConfig) CustomContext where
  getter = smtpConfig
  modifier f ctx = ctx {smtpConfig = f (smtpConfig ctx)}

instance Has.Has PlayoutSecret CustomContext where
  getter = playoutSecret
  modifier f ctx = ctx {playoutSecret = f (playoutSecret ctx)}

instance Has.Has CleanupInterval CustomContext where
  getter = cleanupInterval
  modifier f ctx = ctx {cleanupInterval = f (cleanupInterval ctx)}

instance Has.Has WebhookConfig CustomContext where
  getter = webhookConfig
  modifier f ctx = ctx {webhookConfig = f (webhookConfig ctx)}

instance Has.Has StorageBackend CustomContext where
  getter = storageBackend . storageContext
  modifier f ctx =
    ctx
      { storageContext =
          (storageContext ctx)
            { storageBackend = f (storageBackend (storageContext ctx))
            }
      }

instance Has.Has (Maybe AWS.Env) CustomContext where
  getter = awsEnv . storageContext
  modifier f ctx =
    ctx
      { storageContext =
          (storageContext ctx)
            { awsEnv = f (awsEnv (storageContext ctx))
            }
      }

instance Has.Has (Maybe GoogleAnalyticsId) CustomContext where
  getter = googleAnalyticsId . analyticsConfig
  modifier f ctx =
    ctx
      { analyticsConfig =
          (analyticsConfig ctx)
            { googleAnalyticsId = f (googleAnalyticsId (analyticsConfig ctx))
            }
      }

--------------------------------------------------------------------------------
-- Orphan Has instances for AppContext CustomContext

instance Has.Has StorageContext (AppContext CustomContext) where
  getter = storageContext . appCustom
  modifier f ctx = ctx {appCustom = (appCustom ctx) {storageContext = f (storageContext (appCustom ctx))}}

instance Has.Has AnalyticsConfig (AppContext CustomContext) where
  getter = analyticsConfig . appCustom
  modifier f ctx = ctx {appCustom = (appCustom ctx) {analyticsConfig = f (analyticsConfig (appCustom ctx))}}

instance Has.Has StorageBackend (AppContext CustomContext) where
  getter = Has.getter @StorageBackend . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @StorageBackend f (appCustom ctx)}

instance Has.Has (Maybe AWS.Env) (AppContext CustomContext) where
  getter = Has.getter @(Maybe AWS.Env) . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @(Maybe AWS.Env) f (appCustom ctx)}

instance Has.Has (Maybe GoogleAnalyticsId) (AppContext CustomContext) where
  getter = Has.getter @(Maybe GoogleAnalyticsId) . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @(Maybe GoogleAnalyticsId) f (appCustom ctx)}

instance Has.Has (Maybe SmtpConfig) (AppContext CustomContext) where
  getter = Has.getter @(Maybe SmtpConfig) . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @(Maybe SmtpConfig) f (appCustom ctx)}

instance Has.Has PlayoutSecret (AppContext CustomContext) where
  getter = Has.getter @PlayoutSecret . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @PlayoutSecret f (appCustom ctx)}

instance Has.Has CleanupInterval (AppContext CustomContext) where
  getter = Has.getter @CleanupInterval . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @CleanupInterval f (appCustom ctx)}

instance Has.Has WebhookConfig (AppContext CustomContext) where
  getter = Has.getter @WebhookConfig . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @WebhookConfig f (appCustom ctx)}

--------------------------------------------------------------------------------

-- | All custom configurations loaded from environment (Phase 1).
data CustomConfigs = CustomConfigs
  { ccStorageConfig :: S3ConfigResult,
    ccAnalytics :: AnalyticsConfig,
    ccSmtp :: Maybe SmtpConfig,
    ccPlayout :: PlayoutSecret,
    ccCleanup :: CleanupInterval,
    ccWebhook :: WebhookConfig
  }

-- | Load all custom configurations from environment (Phase 1).
--
-- This reads all environment variables but does NOT build resources that
-- require AppContext (like storage context which needs Environment).
loadCustomConfigs :: (MonadIO m) => m CustomConfigs
loadCustomConfigs = do
  storageConfig <- loadStorageConfig
  analytics <- loadAnalyticsConfig
  smtp <- loadSmtpConfig
  playout <- loadPlayoutSecret
  cleanup <- loadCleanupInterval
  webhook <- loadWebhookConfig
  pure
    CustomConfigs
      { ccStorageConfig = storageConfig,
        ccAnalytics = analytics,
        ccSmtp = smtp,
        ccPlayout = playout,
        ccCleanup = cleanup,
        ccWebhook = webhook
      }

-- | Build CustomContext inside withAppResources callback (Phase 2).
--
-- Uses AppContext to determine environment-specific behavior and
-- logs final configuration state using structured logging.
buildCustomContext ::
  AppContext ctx ->
  CustomConfigs ->
  (CustomContext -> IO a) ->
  IO a
buildCustomContext appCtx CustomConfigs {..} action = do
  -- Log analytics, SMTP, cleanup, and webhook configuration state
  logConfigState (appLoggerEnv appCtx) ccAnalytics ccSmtp ccPlayout ccCleanup ccWebhook

  -- Build storage context (which may log and/or fail)
  withStorageContext appCtx ccStorageConfig $ \storage -> do
    let customCtx =
          CustomContext
            { storageContext = storage,
              analyticsConfig = ccAnalytics,
              smtpConfig = ccSmtp,
              playoutSecret = ccPlayout,
              cleanupInterval = ccCleanup,
              webhookConfig = ccWebhook
            }
    action customCtx

-- | Log the configuration state for analytics, SMTP, playout, cleanup, and webhook.
logConfigState :: Log.LoggerEnv -> AnalyticsConfig -> Maybe SmtpConfig -> PlayoutSecret -> CleanupInterval -> WebhookConfig -> IO ()
logConfigState logEnv analytics smtp playout cleanup webhook = do
  time <- getCurrentTime
  Log.logMessageIO logEnv time Log.LogInfo "Custom configuration loaded" $
    Aeson.object
      [ "analytics"
          .= Aeson.object
            [ "enabled" .= isJust (googleAnalyticsId analytics),
              "gtag_id" .= fmap (\(GoogleAnalyticsId gid) -> gid) (googleAnalyticsId analytics)
            ],
        "smtp"
          .= Aeson.object
            [ "enabled" .= isJust smtp,
              "server" .= fmap smtpServer smtp
            ],
        "playout"
          .= Aeson.object
            ["secret_configured" .= isJust (unPlayoutSecret playout)],
        "cleanup"
          .= Aeson.object
            ["interval_seconds" .= unCleanupInterval cleanup],
        "webhook"
          .= case webhook of
            WebhookDisabled ->
              Aeson.object
                [ "enabled" .= False
                ]
            WebhookEnabled {wcBaseUrl = url} ->
              Aeson.object
                [ "enabled" .= True,
                  "base_url" .= url,
                  "secret_configured" .= True
                ]
      ]
  where
    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust Nothing = False

-- | Load the playout secret from environment (Phase 1).
loadPlayoutSecret :: (MonadIO m) => m PlayoutSecret
loadPlayoutSecret = do
  mSecret <- liftIO $ lookupEnv "PLAYOUT_SECRET"
  pure $ PlayoutSecret (Text.pack <$> mSecret)

-- | Load cleanup interval from APP_CLEANUP_INTERVAL_SECONDS env var (Phase 1).
--
-- Falls back to 'BackgroundJobs.defaultCleanupIntervalSeconds' (1 hour) if not set or invalid.
loadCleanupInterval :: (MonadIO m) => m CleanupInterval
loadCleanupInterval = liftIO $ do
  mInterval <- lookupEnv "APP_CLEANUP_INTERVAL_SECONDS"
  let interval = fromMaybe BackgroundJobs.defaultCleanupIntervalSeconds (mInterval >>= readMaybe)
  pure $ CleanupInterval interval

-- | Load webhook configuration from WEBHOOK_URL and WEBHOOK_SECRET env vars (Phase 1).
loadWebhookConfig :: (MonadIO m) => m WebhookConfig
loadWebhookConfig = liftIO $ do
  mUrl <- lookupEnv "WEBHOOK_URL"
  mSecret <- lookupEnv "WEBHOOK_SECRET"
  pure $ case (mUrl, mSecret) of
    (Just url, Just secret) ->
      WebhookEnabled
        { wcBaseUrl = Text.pack url,
          wcSecret = WebhookSecret $ Text.pack secret
        }
    _ -> WebhookDisabled
