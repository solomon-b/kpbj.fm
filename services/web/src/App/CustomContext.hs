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
    WebhookConfig (..),
    StreamConfig (..),
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
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import EasyPost.Types (EasyPostApiKey (..))
import Log qualified
import Mailchimp.Client (MailchimpClient, mkClient)
import Mailchimp.Config (MailchimpApiKey (..), MailchimpAudienceId (..), MailchimpWebhookSecret (..), parseDataCenter)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS qualified as TLS
import Stripe.Types (StripePublishableKey (..), StripeSecretKey (..), StripeWebhookSecret (..))
import System.Environment (lookupEnv)

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

-- | Stream URLs loaded from environment variables.
--
-- These are infrastructure concerns determined by the nginx/icecast config,
-- not editable at runtime.
data StreamConfig = StreamConfig
  { scStreamUrl :: Text,
    scMetadataUrl :: Text
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
    webhookConfig :: WebhookConfig,
    streamConfig :: StreamConfig,
    stripeSecretKey :: Maybe StripeSecretKey,
    stripePublishableKey :: Maybe StripePublishableKey,
    stripeWebhookSecret :: Maybe StripeWebhookSecret,
    easypostApiKey :: Maybe EasyPostApiKey,
    mailchimpClient :: Maybe MailchimpClient,
    mailchimpWebhookSecret :: Maybe MailchimpWebhookSecret,
    httpManager :: Manager
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

instance Has.Has WebhookConfig CustomContext where
  getter = webhookConfig
  modifier f ctx = ctx {webhookConfig = f (webhookConfig ctx)}

instance Has.Has StreamConfig CustomContext where
  getter = streamConfig
  modifier f ctx = ctx {streamConfig = f (streamConfig ctx)}

instance Has.Has (Maybe StripeSecretKey) CustomContext where
  getter = stripeSecretKey
  modifier f ctx = ctx {stripeSecretKey = f (stripeSecretKey ctx)}

instance Has.Has (Maybe StripePublishableKey) CustomContext where
  getter = stripePublishableKey
  modifier f ctx = ctx {stripePublishableKey = f (stripePublishableKey ctx)}

instance Has.Has (Maybe StripeWebhookSecret) CustomContext where
  getter = stripeWebhookSecret
  modifier f ctx = ctx {stripeWebhookSecret = f (stripeWebhookSecret ctx)}

instance Has.Has (Maybe EasyPostApiKey) CustomContext where
  getter = easypostApiKey
  modifier f ctx = ctx {easypostApiKey = f (easypostApiKey ctx)}

instance Has.Has (Maybe MailchimpClient) CustomContext where
  getter = mailchimpClient
  modifier f ctx = ctx {mailchimpClient = f (mailchimpClient ctx)}

instance Has.Has (Maybe MailchimpWebhookSecret) CustomContext where
  getter = mailchimpWebhookSecret
  modifier f ctx = ctx {mailchimpWebhookSecret = f (mailchimpWebhookSecret ctx)}

instance Has.Has Manager CustomContext where
  getter = httpManager
  modifier f ctx = ctx {httpManager = f (httpManager ctx)}

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

instance Has.Has WebhookConfig (AppContext CustomContext) where
  getter = Has.getter @WebhookConfig . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @WebhookConfig f (appCustom ctx)}

instance Has.Has StreamConfig (AppContext CustomContext) where
  getter = Has.getter @StreamConfig . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @StreamConfig f (appCustom ctx)}

instance Has.Has (Maybe StripeSecretKey) (AppContext CustomContext) where
  getter = Has.getter @(Maybe StripeSecretKey) . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @(Maybe StripeSecretKey) f (appCustom ctx)}

instance Has.Has (Maybe StripePublishableKey) (AppContext CustomContext) where
  getter = Has.getter @(Maybe StripePublishableKey) . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @(Maybe StripePublishableKey) f (appCustom ctx)}

instance Has.Has (Maybe StripeWebhookSecret) (AppContext CustomContext) where
  getter = Has.getter @(Maybe StripeWebhookSecret) . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @(Maybe StripeWebhookSecret) f (appCustom ctx)}

instance Has.Has (Maybe EasyPostApiKey) (AppContext CustomContext) where
  getter = Has.getter @(Maybe EasyPostApiKey) . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @(Maybe EasyPostApiKey) f (appCustom ctx)}

instance Has.Has (Maybe MailchimpClient) (AppContext CustomContext) where
  getter = Has.getter @(Maybe MailchimpClient) . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @(Maybe MailchimpClient) f (appCustom ctx)}

instance Has.Has (Maybe MailchimpWebhookSecret) (AppContext CustomContext) where
  getter = Has.getter @(Maybe MailchimpWebhookSecret) . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @(Maybe MailchimpWebhookSecret) f (appCustom ctx)}

instance Has.Has Manager (AppContext CustomContext) where
  getter = Has.getter @Manager . appCustom
  modifier f ctx = ctx {appCustom = Has.modifier @Manager f (appCustom ctx)}

--------------------------------------------------------------------------------

-- | All custom configurations loaded from environment (Phase 1).
data CustomConfigs = CustomConfigs
  { ccStorageConfig :: S3ConfigResult,
    ccAnalytics :: AnalyticsConfig,
    ccSmtp :: Maybe SmtpConfig,
    ccPlayout :: PlayoutSecret,
    ccWebhook :: WebhookConfig,
    ccStream :: StreamConfig,
    ccStripeSecretKey :: Maybe StripeSecretKey,
    ccStripePublishableKey :: Maybe StripePublishableKey,
    ccStripeWebhookSecret :: Maybe StripeWebhookSecret,
    ccEasypostApiKey :: Maybe EasyPostApiKey,
    ccMailchimpApiKey :: Maybe MailchimpApiKey,
    ccMailchimpAudienceId :: Maybe MailchimpAudienceId,
    ccMailchimpWebhookSecret :: Maybe MailchimpWebhookSecret
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
  webhook <- loadWebhookConfig
  stream <- loadStreamConfig
  mStripeSecret <- liftIO $ fmap (StripeSecretKey . TE.encodeUtf8 . Text.pack) <$> lookupEnv "STRIPE_SECRET_KEY"
  mStripePubKey <- liftIO $ fmap (StripePublishableKey . Text.pack) <$> lookupEnv "STRIPE_PUBLISHABLE_KEY"
  mStripeWebhookSecret <- liftIO $ fmap (StripeWebhookSecret . TE.encodeUtf8 . Text.pack) <$> lookupEnv "STRIPE_WEBHOOK_SECRET"
  mEasypostKey <- liftIO $ fmap (EasyPostApiKey . TE.encodeUtf8 . Text.pack) <$> lookupEnv "EASYPOST_API_KEY"
  mMailchimpApiKey <- liftIO $ fmap (MailchimpApiKey . TE.encodeUtf8 . Text.pack) <$> lookupEnv "MAILCHIMP_API_KEY"
  mMailchimpAudienceId <- liftIO $ fmap (MailchimpAudienceId . Text.pack) <$> lookupEnv "MAILCHIMP_AUDIENCE_ID"
  mMailchimpWebhookSecret <- liftIO $ fmap (MailchimpWebhookSecret . Text.pack) <$> lookupEnv "MAILCHIMP_WEBHOOK_SECRET"
  pure
    CustomConfigs
      { ccStorageConfig = storageConfig,
        ccAnalytics = analytics,
        ccSmtp = smtp,
        ccPlayout = playout,
        ccWebhook = webhook,
        ccStream = stream,
        ccStripeSecretKey = mStripeSecret,
        ccStripePublishableKey = mStripePubKey,
        ccStripeWebhookSecret = mStripeWebhookSecret,
        ccEasypostApiKey = mEasypostKey,
        ccMailchimpApiKey = mMailchimpApiKey,
        ccMailchimpAudienceId = mMailchimpAudienceId,
        ccMailchimpWebhookSecret = mMailchimpWebhookSecret
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
  -- Log analytics, SMTP, webhook, and stream configuration state
  logConfigState (appLoggerEnv appCtx) ccAnalytics ccSmtp ccPlayout ccWebhook ccStream

  -- Create TLS-capable HTTP manager for external API calls
  mgr <- TLS.newTlsManager

  -- Build the Mailchimp client when both credentials are present.
  -- Failure to parse the data center suffix logs and disables the client.
  mailchimp <- buildMailchimpClient (appLoggerEnv appCtx) mgr ccMailchimpApiKey ccMailchimpAudienceId

  -- Loud banner for the inbound webhook secret. The handler at
  -- API.Webhooks.Mailchimp.Post.Handler 204s without processing when this is
  -- 'Nothing'; surface it at boot so a missed prod rotation is visible.
  webhookTime <- getCurrentTime
  case ccMailchimpWebhookSecret of
    Just _ ->
      Log.logMessageIO
        (appLoggerEnv appCtx)
        webhookTime
        Log.LogInfo
        "Mailchimp webhook secret configured"
        (Aeson.object ["enabled" .= True])
    Nothing ->
      Log.logMessageIO
        (appLoggerEnv appCtx)
        webhookTime
        Log.LogAttention
        "MAILCHIMP WEBHOOK DISABLED — POST /api/webhooks/mailchimp will 204 without processing (no MAILCHIMP_WEBHOOK_SECRET)"
        (Aeson.object ["enabled" .= False])

  -- Build storage context (which may log and/or fail)
  withStorageContext appCtx ccStorageConfig $ \storage -> do
    let customCtx =
          CustomContext
            { storageContext = storage,
              analyticsConfig = ccAnalytics,
              smtpConfig = ccSmtp,
              playoutSecret = ccPlayout,
              webhookConfig = ccWebhook,
              streamConfig = ccStream,
              stripeSecretKey = ccStripeSecretKey,
              stripePublishableKey = ccStripePublishableKey,
              stripeWebhookSecret = ccStripeWebhookSecret,
              easypostApiKey = ccEasypostApiKey,
              mailchimpClient = mailchimp,
              mailchimpWebhookSecret = ccMailchimpWebhookSecret,
              httpManager = mgr
            }
    action customCtx

-- | Build a 'MailchimpClient' from the loaded credentials.
--
-- Returns 'Nothing' when either credential is missing (the dev / unconfigured
-- case), or when the API key's data center suffix cannot be parsed (logged
-- as a warning so a typo does not fail the boot but is still visible).
buildMailchimpClient ::
  Log.LoggerEnv ->
  Manager ->
  Maybe MailchimpApiKey ->
  Maybe MailchimpAudienceId ->
  IO (Maybe MailchimpClient)
buildMailchimpClient logEnv mgr mKey mAudience = do
  time <- getCurrentTime
  case (mKey, mAudience) of
    (Just key, Just audience@(MailchimpAudienceId audienceId)) ->
      case mkClient mgr key audience of
        Right client -> do
          let dc = fromRight "?" (parseDataCenter key)
          Log.logMessageIO
            logEnv
            time
            Log.LogInfo
            "Mailchimp client configured"
            ( Aeson.object
                [ "enabled" .= True,
                  "data_center" .= dc,
                  "audience_id" .= audienceId
                ]
            )
          pure (Just client)
        Left err -> do
          Log.logMessageIO logEnv time Log.LogAttention "Mailchimp client could not be built" (Aeson.object ["error" .= err])
          pure Nothing
    _ -> do
      Log.logMessageIO
        logEnv
        time
        Log.LogAttention
        "MAILCHIMP DISABLED — outbound sync is a no-op (no MAILCHIMP_API_KEY or MAILCHIMP_AUDIENCE_ID)"
        (Aeson.object ["enabled" .= False])
      pure Nothing

-- | Log the configuration state for analytics, SMTP, playout, webhook, stream, Stripe, and EasyPost.
logConfigState :: Log.LoggerEnv -> AnalyticsConfig -> Maybe SmtpConfig -> PlayoutSecret -> WebhookConfig -> StreamConfig -> IO ()
logConfigState logEnv analytics smtp playout webhook stream = do
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
                ],
        "stream"
          .= Aeson.object
            [ "stream_url" .= scStreamUrl stream,
              "metadata_url" .= scMetadataUrl stream
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

-- | Load stream configuration from STREAM_URL and METADATA_URL env vars (Phase 1).
--
-- Both variables are required. Crashes at startup if either is missing.
loadStreamConfig :: (MonadIO m) => m StreamConfig
loadStreamConfig = liftIO $ do
  mStreamUrl <- lookupEnv "STREAM_URL"
  mMetadataUrl <- lookupEnv "METADATA_URL"
  case (mStreamUrl, mMetadataUrl) of
    (Just streamUrl, Just metadataUrl) ->
      pure
        StreamConfig
          { scStreamUrl = Text.pack streamUrl,
            scMetadataUrl = Text.pack metadataUrl
          }
    _ -> error "STREAM_URL and METADATA_URL environment variables are required"
