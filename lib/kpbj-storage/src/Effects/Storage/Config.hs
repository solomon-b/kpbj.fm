-- | Storage configuration from the environment, and the choice of backend.
module Effects.Storage.Config
  ( -- * Loading
    S3ConfigResult (..),
    loadStorageConfig,

    -- * Backend choice
    StorageConfigError (..),
    selectStorageBackend,
  )
where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.StorageBackend (S3StorageConfig (..), StorageBackend (..), defaultLocalConfig)
import System.Environment (lookupEnv)

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
-- Reads: BUCKET_NAME, AWS_REGION, AWS_ENDPOINT_URL_S3, S3_BASE_URL
-- Does NOT determine backend - that requires Environment from AppContext.
loadStorageConfig :: (MonadIO m) => m S3ConfigResult
loadStorageConfig = liftIO $ do
  mBucket <- lookupEnvText "BUCKET_NAME"
  mRegion <- lookupEnvText "AWS_REGION"
  mEndpoint <- lookupEnvText "AWS_ENDPOINT_URL_S3"
  mBaseUrl <- lookupEnvText "S3_BASE_URL"

  case (mBucket, mRegion) of
    -- All required vars set - valid configuration
    (Just bucket, Just region) -> do
      let baseUrl = case mBaseUrl of
            -- Explicit base URL override (for custom domains, CDNs, etc.)
            Just url -> Text.dropWhileEnd (== '/') url
            Nothing -> case mEndpoint of
              -- S3-compatible: derive base URL from endpoint (bucket subdomain)
              Just endpoint -> "https://" <> bucket <> "." <> stripScheme endpoint
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

--------------------------------------------------------------------------------

-- | A storage configuration that cannot run.
data StorageConfigError
  = -- | Some S3 variables are set and some are not. The first list holds
    -- the missing names. The second list holds the present names.
    PartialS3ConfigError [Text] [Text]
  | -- | Staging or Production has no S3 variables.
    MissingS3ConfigError Environment
  deriving stock (Show)

-- | Choose the storage backend for an environment.
--
-- A partial S3 configuration is always an error. Development always uses
-- local storage, even when the S3 variables are set. Staging and
-- Production need a full S3 configuration.
selectStorageBackend ::
  Environment ->
  S3ConfigResult ->
  Either StorageConfigError StorageBackend
selectStorageBackend env s3ConfigResult =
  case (env, s3ConfigResult) of
    (_, PartialS3Config missing present) -> Left (PartialS3ConfigError missing present)
    (Development, _) -> Right (LocalStorage defaultLocalConfig)
    (_, ValidS3Config s3Config) -> Right (S3Storage s3Config)
    (_, NoS3Config) -> Left (MissingS3ConfigError env)

--------------------------------------------------------------------------------

-- | Look up an environment variable and convert to Text.
lookupEnvText :: String -> IO (Maybe Text)
lookupEnvText name = do
  mVal <- lookupEnv name
  pure $ Text.pack <$> mVal

-- | Strip the scheme (https:// or http://) from a URL.
stripScheme :: Text -> Text
stripScheme url
  | "https://" `Text.isPrefixOf` url = Text.drop 8 url
  | "http://" `Text.isPrefixOf` url = Text.drop 7 url
  | otherwise = url
