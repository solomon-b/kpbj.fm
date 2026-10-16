{-# OPTIONS_GHC -Wno-orphans #-}

module App.Storage
  ( -- * Storage Context
    StorageContext (..),

    -- * Two-phase initialization
    S3ConfigResult (..),
    loadStorageConfig,
    withStorageContext,

    -- * Re-exports
    StorageBackend (..),
    LocalStorageConfig (..),
    S3StorageConfig (..),
    buildMediaUrl,
    defaultLocalConfig,
  )
where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import App.Context (AppContext (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Domain.Types.StorageBackend
import Effects.AWS qualified as AWS
import Effects.Storage.Config (S3ConfigResult (..), StorageConfigError (..), loadStorageConfig, selectStorageBackend)
import Log qualified

--------------------------------------------------------------------------------

-- | Storage context containing backend configuration and AWS environment.
--
-- This is accessed via the 'Has' typeclass in handlers.
data StorageContext = StorageContext
  { storageBackend :: StorageBackend,
    awsEnv :: Maybe AWS.Env
  }

instance Has.Has StorageBackend StorageContext where
  getter = storageBackend
  modifier f ctx = ctx {storageBackend = f (storageBackend ctx)}

instance Has.Has (Maybe AWS.Env) StorageContext where
  getter = awsEnv
  modifier f ctx = ctx {awsEnv = f (awsEnv ctx)}

--------------------------------------------------------------------------------

-- | Build storage context with AppContext (Phase 2).
--
-- 'selectStorageBackend' chooses the backend from appEnvironment and the
-- S3 configuration. appLoggerEnv receives the log messages.
--
-- Logic:
--   - Development + any config → LocalStorage (always)
--   - Staging/Production + ValidS3Config → S3Storage (init AWS.Env)
--   - Staging/Production + NoS3Config → Fatal error (logged)
--   - Any + PartialS3Config → Fatal error (logged)
withStorageContext ::
  AppContext ctx ->
  S3ConfigResult ->
  (StorageContext -> IO a) ->
  IO a
withStorageContext appCtx s3ConfigResult action = do
  let logEnv = appLoggerEnv appCtx
      env = appEnvironment appCtx

  case selectStorageBackend env s3ConfigResult of
    -- Partial config is always an error, regardless of environment
    Left (PartialS3ConfigError missing present) -> do
      logStorageError logEnv "Partial S3 configuration detected" $
        Aeson.object
          [ "missing_vars" .= missing,
            "present_vars" .= present,
            "hint" .= ("Set all required env vars: BUCKET_NAME, AWS_REGION" :: Text)
          ]
      error "Fatal: Partial S3 configuration"
    -- Staging/Production without S3 config
    Left (MissingS3ConfigError _) -> do
      logStorageError logEnv "S3 configuration required" $
        Aeson.object
          [ "environment" .= show env,
            "required_vars" .= (["BUCKET_NAME", "AWS_REGION"] :: [Text]),
            "hint" .= ("For S3-compatible services (DigitalOcean Spaces, MinIO), also set AWS_ENDPOINT_URL_S3" :: Text)
          ]
      error $ "Fatal: S3 configuration required for " <> show env
    -- Development always uses local storage
    Right (LocalStorage localConfig) -> do
      logStorageInit logEnv "local" $
        Aeson.object ["environment" .= show env]
      action
        StorageContext
          { storageBackend = LocalStorage localConfig,
            awsEnv = Nothing
          }
    -- Staging/Production with valid S3 config
    Right (S3Storage s3Config) -> do
      awsEnv' <- AWS.initAWSEnv (s3Region s3Config) (s3EndpointUrl s3Config)
      logStorageInit logEnv "s3" $
        Aeson.object
          [ "bucket" .= s3BucketName s3Config,
            "region" .= s3Region s3Config,
            "endpoint" .= s3EndpointUrl s3Config,
            "environment" .= show env
          ]
      action
        StorageContext
          { storageBackend = S3Storage s3Config,
            awsEnv = Just awsEnv'
          }

-- | Log storage initialization with structured logging.
logStorageInit :: Log.LoggerEnv -> Text -> Aeson.Value -> IO ()
logStorageInit logEnv backend details = do
  time <- getCurrentTime
  Log.logMessageIO logEnv time Log.LogInfo "Storage initialized" $
    Aeson.object ["backend" .= backend, "details" .= details]

-- | Log storage error with structured logging.
logStorageError :: Log.LoggerEnv -> Text -> Aeson.Value -> IO ()
logStorageError logEnv message details = do
  time <- getCurrentTime
  Log.logMessageIO logEnv time Log.LogAttention message details
