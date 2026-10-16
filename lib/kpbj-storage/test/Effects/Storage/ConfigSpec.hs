module Effects.Storage.ConfigSpec (spec) where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import Domain.Types.StorageBackend (S3StorageConfig (..), StorageBackend (..), defaultLocalConfig)
import Effects.Storage.Config (S3ConfigResult (..), StorageConfigError (..), selectStorageBackend)
import Test.Hspec

--------------------------------------------------------------------------------

s3Config :: S3StorageConfig
s3Config =
  S3StorageConfig
    { s3BucketName = "kpbj-storage",
      s3Region = "sfo3",
      s3BaseUrl = "https://kpbj-storage.sfo3.digitaloceanspaces.com",
      s3EndpointUrl = Just "https://sfo3.digitaloceanspaces.com"
    }

spec :: Spec
spec = describe "selectStorageBackend" $ do
  it "uses local storage in Development when S3 is not configured" $
    selectStorageBackend Development NoS3Config `shouldSatisfy` isLocal

  it "uses local storage in Development even when S3 is configured" $
    selectStorageBackend Development (ValidS3Config s3Config) `shouldSatisfy` isLocal

  it "uses S3 in Staging when S3 is configured" $
    selectStorageBackend Staging (ValidS3Config s3Config) `shouldSatisfy` isS3

  it "uses S3 in Production when S3 is configured" $
    selectStorageBackend Production (ValidS3Config s3Config) `shouldSatisfy` isS3

  it "refuses Production when S3 is not configured" $
    selectStorageBackend Production NoS3Config `shouldSatisfy` isMissing

  it "refuses Staging when S3 is not configured" $
    selectStorageBackend Staging NoS3Config `shouldSatisfy` isMissing

  it "refuses a partial S3 configuration in Development" $
    selectStorageBackend Development (PartialS3Config ["AWS_REGION"] ["BUCKET_NAME"]) `shouldSatisfy` isPartial

  it "refuses a partial S3 configuration in Production" $
    selectStorageBackend Production (PartialS3Config ["AWS_REGION"] ["BUCKET_NAME"]) `shouldSatisfy` isPartial
  where
    isLocal = \case
      Right (LocalStorage config) -> config == defaultLocalConfig
      _ -> False
    isS3 = \case
      Right (S3Storage config) -> config == s3Config
      _ -> False
    isMissing = \case
      Left (MissingS3ConfigError _) -> True
      _ -> False
    isPartial = \case
      Left (PartialS3ConfigError ["AWS_REGION"] ["BUCKET_NAME"]) -> True
      _ -> False
