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
import App.Config (Environment (..))
import App.Context (AppContext (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Domain.Types.StorageBackend
import Effects.AWS qualified as AWS
import Log qualified
import System.Environment (lookupEnv)

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

-- | S3 configuration result.
data S3ConfigResult
  = -- | No S3 env vars set (fatal in Production/Staging, ignored in Development)
    NoS3Config
  | -- | All required S3 env vars set
    ValidS3Config S3StorageConfig
  | -- | Some but not all S3 env vars set - configuration error
    PartialS3Config
      [Text]
      -- | (missing vars, present vars)
      [Text]

-- | Load storage configuration from environment (Phase 1).
--
-- Reads: BUCKET_NAME, AWS_REGION, AWS_ENDPOINT_URL_S3
-- Does NOT determine backend - that requires Environment from AppContext.
loadStorageConfig :: (MonadIO m) => m S3ConfigResult
loadStorageConfig = liftIO $ do
  mBucket <- lookupEnvText "BUCKET_NAME"
  mRegion <- lookupEnvText "AWS_REGION"
  mEndpoint <- lookupEnvText "AWS_ENDPOINT_URL_S3"

  case (mBucket, mRegion) of
    -- All required vars set - valid configuration
    (Just bucket, Just region) -> do
      -- Construct base URL based on whether we have a custom endpoint
      let baseUrl = case mEndpoint of
            -- Tigris/S3-compatible: use bucket subdomain on Tigris
            Just _ -> "https://" <> bucket <> ".fly.storage.tigris.dev"
            -- Standard AWS S3: use standard URL format
            Nothing -> "https://" <> bucket <> ".s3." <> region <> ".amazonaws.com"
      pure $
        ValidS3Config
          S3StorageConfig
            { s3BucketName = bucket,
              s3Region = region,
              s3BaseUrl = baseUrl,
              s3EndpointUrl = mEndpoint
            }
    -- No vars set - no S3 configured
    (Nothing, Nothing) ->
      pure NoS3Config
    -- Partial configuration - error
    _ -> do
      let envVars =
            [ ("BUCKET_NAME", mBucket),
              ("AWS_REGION", mRegion)
            ]
          missing = [name | (name, Nothing) <- envVars]
          present = [name | (name, Just _) <- envVars]
      pure $ PartialS3Config missing present

-- | Build storage context with AppContext (Phase 2).
--
-- Uses appEnvironment to decide Local vs S3, appLoggerEnv for logging.
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

  case (env, s3ConfigResult) of
    -- Partial config is always an error, regardless of environment
    (_, PartialS3Config missing present) -> do
      logStorageError logEnv "Partial S3 configuration detected" $
        Aeson.object
          [ "missing_vars" .= missing,
            "present_vars" .= present,
            "hint" .= ("Set all required env vars: BUCKET_NAME, AWS_REGION" :: Text)
          ]
      error "Fatal: Partial S3 configuration"
    -- Development always uses local storage
    (Development, _) -> do
      logStorageInit logEnv "local" $
        Aeson.object ["environment" .= ("Development" :: Text)]
      action
        StorageContext
          { storageBackend = LocalStorage defaultLocalConfig,
            awsEnv = Nothing
          }
    -- Staging/Production with valid S3 config
    (_, ValidS3Config s3Config) -> do
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
    -- Staging/Production without S3 config
    (_, NoS3Config) -> do
      logStorageError logEnv "S3 configuration required" $
        Aeson.object
          [ "environment" .= show env,
            "required_vars" .= (["BUCKET_NAME", "AWS_REGION"] :: [Text]),
            "hint" .= ("For S3-compatible services (Tigris, MinIO), also set AWS_ENDPOINT_URL_S3" :: Text)
          ]
      error $ "Fatal: S3 configuration required for " <> show env

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

-- | Look up an environment variable and convert to Text.
lookupEnvText :: String -> IO (Maybe Text)
lookupEnvText name = do
  mVal <- lookupEnv name
  pure $ Text.pack <$> mVal
