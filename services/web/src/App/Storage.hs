{-# OPTIONS_GHC -Wno-orphans #-}

module App.Storage
  ( -- * Storage Context
    StorageContext (..),
    initStorageContext,

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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.StorageBackend
import Effects.AWS qualified as AWS
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

-- | Initialize storage context based on environment variables.
--
-- Uses S3 storage if BUCKET_NAME and AWS_REGION are set, otherwise falls back
-- to local filesystem storage.
--
-- Environment variables (set automatically by `fly storage create --app`):
--   - BUCKET_NAME: S3 bucket name (required for S3)
--   - AWS_REGION: AWS region, typically "auto" for Tigris (required for S3)
--   - AWS_ENDPOINT_URL_S3: Custom S3 endpoint (optional, for S3-compatible services)
--   - AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY: Credentials (auto-discovered by SDK)
--
-- For standard AWS S3, omit AWS_ENDPOINT_URL_S3 and the SDK will use default endpoints.
--
-- IMPORTANT: If some but not all S3 env vars are set, the application will fail
-- to start. This prevents accidental data loss from misconfiguration.
initStorageContext :: (MonadIO m) => m StorageContext
initStorageContext = do
  s3ConfigResult <- loadS3Config
  case s3ConfigResult of
    NoS3Config -> do
      liftIO $ putStrLn "Using local storage (no S3 env vars set)"
      pure
        StorageContext
          { storageBackend = LocalStorage defaultLocalConfig,
            awsEnv = Nothing
          }
    ValidS3Config s3Config -> do
      liftIO $ putStrLn $ "Using S3 storage (bucket: " <> Text.unpack (s3BucketName s3Config) <> ")"
      liftIO $ case s3EndpointUrl s3Config of
        Just endpoint -> putStrLn $ "  Custom endpoint: " <> Text.unpack endpoint
        Nothing -> putStrLn "  Using standard AWS S3 endpoint"
      awsEnv' <- AWS.initAWSEnv (s3Region s3Config) (s3EndpointUrl s3Config)
      pure
        StorageContext
          { storageBackend = S3Storage s3Config,
            awsEnv = Just awsEnv'
          }
    PartialS3Config missing present -> do
      liftIO $ do
        putStrLn "ERROR: Partial S3 configuration detected!"
        putStrLn $ "  Missing env vars: " <> Text.unpack (Text.intercalate ", " missing)
        putStrLn $ "  Present env vars: " <> Text.unpack (Text.intercalate ", " present)
        putStrLn ""
        putStrLn "Set all required env vars: BUCKET_NAME, AWS_REGION"
        putStrLn "For S3-compatible services (Tigris, MinIO), also set AWS_ENDPOINT_URL_S3"
        putStrLn "Or set NONE to use local storage."
      error "Fatal: Partial S3 configuration"

-- | S3 configuration result.
data S3ConfigResult
  = -- | No S3 env vars set - allows fallback to local storage
    NoS3Config
  | -- | All required S3 env vars set
    ValidS3Config S3StorageConfig
  | -- | Some but not all S3 env vars set - configuration error
    PartialS3Config
      [Text]
      -- | (missing vars, present vars)
      [Text]

-- | Load S3 configuration from environment variables.
--
-- Required env vars (set by `fly storage create --app` for Tigris):
--   - BUCKET_NAME: Bucket name
--   - AWS_REGION: Region (typically "auto" for Tigris, or standard AWS region)
--
-- Optional env var:
--   - AWS_ENDPOINT_URL_S3: Custom endpoint URL for S3-compatible services
--     When set, base URL is auto-constructed as: https://{bucket}.fly.storage.tigris.dev
--     When not set, uses standard AWS S3 URL format
--
-- Returns 'NoS3Config' if none of the S3 env vars are set.
-- Returns 'ValidS3Config' if all required vars are present.
-- Returns 'PartialS3Config' if some but not all vars are set,
-- which should be treated as a configuration error.
loadS3Config :: (MonadIO m) => m S3ConfigResult
loadS3Config = liftIO $ do
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

-- | Look up an environment variable and convert to Text.
lookupEnvText :: String -> IO (Maybe Text)
lookupEnvText name = do
  mVal <- lookupEnv name
  pure $ Text.pack <$> mVal
