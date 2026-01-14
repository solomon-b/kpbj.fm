module Effects.AWS
  ( -- * AWS Environment
    initAWSEnv,
  )
where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEnc

--------------------------------------------------------------------------------

-- | Initialize an AWS environment for S3 operations.
--
-- Uses the standard AWS SDK credential discovery mechanism:
-- 1. Environment variables (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY)
-- 2. AWS credentials file (~/.aws/credentials)
-- 3. IAM role (when running on AWS infrastructure like EC2, ECS, Lambda)
-- 4. Container credentials (when running in ECS/Fargate)
--
-- The region is set from the provided configuration.
-- Optionally accepts a custom endpoint URL for S3-compatible services like Tigris.
initAWSEnv ::
  (MonadIO m) =>
  -- | AWS region (e.g., "us-west-2" or "auto" for Tigris)
  Text ->
  -- | Optional custom S3 endpoint URL (e.g., "https://fly.storage.tigris.dev")
  Maybe Text ->
  m AWS.Env
initAWSEnv region mEndpointUrl = liftIO $ do
  env <- AWS.newEnv AWS.discover
  let awsRegion = AWS.Region' region
      baseEnv = env {AWS.region = awsRegion}
  case mEndpointUrl of
    Nothing -> pure baseEnv
    Just endpointUrl -> do
      -- Configure custom endpoint for S3-compatible services (e.g., Tigris, MinIO, LocalStack)
      -- Uses path-style addressing which is required by most S3-compatible services
      let (isSecure, host) = parseEndpoint endpointUrl
          customEndpoint = AWS.setEndpoint isSecure (TextEnc.encodeUtf8 host) 443
      pure $ AWS.overrideService customEndpoint baseEnv

-- | Parse an endpoint URL into (isSecure, host) components.
--
-- >>> parseEndpoint "https://fly.storage.tigris.dev"
-- (True, "fly.storage.tigris.dev")
--
-- >>> parseEndpoint "http://localhost:9000"
-- (False, "localhost:9000")
parseEndpoint :: Text -> (Bool, Text)
parseEndpoint url
  | "https://" `Text.isPrefixOf` url = (True, Text.drop 8 url)
  | "http://" `Text.isPrefixOf` url = (False, Text.drop 7 url)
  | otherwise = (True, url) -- Default to HTTPS
