module Domain.Types.StorageBackendSpec (spec) where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.StorageBackend
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------
-- Generators

genLocalStorageConfig :: (MonadGen m) => m LocalStorageConfig
genLocalStorageConfig = do
  root <- Gen.string (Range.linear 1 50) Gen.alphaNum
  pure $ LocalStorageConfig {localStorageRoot = "/" <> root}

genS3StorageConfig :: (MonadGen m) => m S3StorageConfig
genS3StorageConfig = do
  bucketName <- Gen.text (Range.linear 3 30) Gen.alphaNum
  region <- Gen.element ["us-west-2", "us-east-1", "eu-west-1", "auto"]
  let baseUrl = "https://" <> bucketName <> ".s3." <> region <> ".amazonaws.com"
  useCustomEndpoint <- Gen.bool
  endpointUrl <-
    if useCustomEndpoint
      then Just <$> Gen.text (Range.linear 10 50) Gen.alphaNum
      else pure Nothing
  pure $
    S3StorageConfig
      { s3BucketName = bucketName,
        s3Region = region,
        s3BaseUrl = baseUrl,
        s3EndpointUrl = endpointUrl
      }

genStorageBackend :: (MonadGen m) => m StorageBackend
genStorageBackend =
  Gen.choice
    [ LocalStorage <$> genLocalStorageConfig,
      S3Storage <$> genS3StorageConfig
    ]

-- | Generate a valid object key (file path within storage)
genObjectKey :: (MonadGen m) => m Text
genObjectKey = do
  -- Generate path segments like "images/2024/01/01/logos/file.jpg"
  numSegments <- Gen.int (Range.linear 1 5)
  segments <- Gen.list (Range.singleton numSegments) genPathSegment
  extension <- Gen.element [".jpg", ".png", ".mp3", ".wav", ""]
  pure $ Text.intercalate "/" segments <> extension
  where
    genPathSegment = Gen.text (Range.linear 1 20) Gen.alphaNum

--------------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = describe "Domain.Types.StorageBackend" $ do
  describe "buildMediaUrl" $ do
    describe "LocalStorage" $ do
      it "always starts with /media/" $ hedgehog $ do
        config <- forAll genLocalStorageConfig
        key <- forAll genObjectKey
        let url = buildMediaUrl (LocalStorage config) key
        assert $ Text.isPrefixOf "/media/" url

      it "contains the object key" $ hedgehog $ do
        config <- forAll genLocalStorageConfig
        key <- forAll genObjectKey
        let url = buildMediaUrl (LocalStorage config) key
        assert $ Text.isInfixOf key url

      it "produces /media/{key} format" $ hedgehog $ do
        config <- forAll genLocalStorageConfig
        key <- forAll genObjectKey
        let url = buildMediaUrl (LocalStorage config) key
        url === "/media/" <> key

    describe "S3Storage" $ do
      it "starts with the configured base URL" $ hedgehog $ do
        config <- forAll genS3StorageConfig
        key <- forAll genObjectKey
        let url = buildMediaUrl (S3Storage config) key
        assert $ Text.isPrefixOf (s3BaseUrl config) url

      it "contains the object key" $ hedgehog $ do
        config <- forAll genS3StorageConfig
        key <- forAll genObjectKey
        let url = buildMediaUrl (S3Storage config) key
        assert $ Text.isInfixOf key url

      it "produces {baseUrl}/{key} format" $ hedgehog $ do
        config <- forAll genS3StorageConfig
        key <- forAll genObjectKey
        let url = buildMediaUrl (S3Storage config) key
        url === s3BaseUrl config <> "/" <> key

    describe "General properties" $ do
      it "URL always contains the object key" $ hedgehog $ do
        backend <- forAll genStorageBackend
        key <- forAll genObjectKey
        let url = buildMediaUrl backend key
        assert $ Text.isInfixOf key url

      it "URL is never empty when key is non-empty" $ hedgehog $ do
        backend <- forAll genStorageBackend
        key <- forAll $ Gen.filter (not . Text.null) genObjectKey
        let url = buildMediaUrl backend key
        assert $ not (Text.null url)

      it "different backends produce different URL schemes" $ hedgehog $ do
        localConfig <- forAll genLocalStorageConfig
        s3Config <- forAll genS3StorageConfig
        key <- forAll genObjectKey
        let localUrl = buildMediaUrl (LocalStorage localConfig) key
            s3Url = buildMediaUrl (S3Storage s3Config) key
        -- Local URLs start with /media/, S3 URLs start with https://
        assert $ Text.isPrefixOf "/media/" localUrl
        assert $ Text.isPrefixOf "https://" s3Url

  describe "defaultLocalConfig" $ do
    it "uses /tmp/kpbj as root" $ do
      localStorageRoot defaultLocalConfig `shouldBe` "/tmp/kpbj"
