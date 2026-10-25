module App.BaseUrlSpec (spec) where

--------------------------------------------------------------------------------

import App.BaseUrl (absoluteMediaUrl)
import Data.Text (Text)
import Domain.Types.StorageBackend (LocalStorageConfig (..), S3StorageConfig (..), StorageBackend (..))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------
-- Fixtures

s3Backend :: StorageBackend
s3Backend =
  S3Storage
    S3StorageConfig
      { s3BucketName = "production-kpbj-storage",
        s3Region = "sfo3",
        s3BaseUrl = "https://production-kpbj-storage.sfo3.digitaloceanspaces.com",
        s3EndpointUrl = Just "https://sfo3.digitaloceanspaces.com"
      }

localBackend :: StorageBackend
localBackend = LocalStorage (LocalStorageConfig "/tmp/kpbj")

episodeKey :: Text
episodeKey = "audio/episodes/2026/10/12/34-parallels_2026-10-12_5d829b7e985e4fd5a40953ea40511104.mp3"

--------------------------------------------------------------------------------
-- Generators

genSiteBaseUrl :: (MonadGen m) => m Text
genSiteBaseUrl =
  Gen.element
    [ "http://localhost:4000",
      "https://staging.kpbj.fm",
      "https://www.kpbj.fm"
    ]

genObjectKey :: (MonadGen m) => m Text
genObjectKey = Gen.text (Range.linear 1 80) (Gen.choice [Gen.alphaNum, Gen.element ['/', '_', '-', '.']])

--------------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = describe "App.BaseUrl" $ do
  describe "absoluteMediaUrl" $ do
    it "gives the bucket URL for S3 storage" $ do
      absoluteMediaUrl "https://www.kpbj.fm" s3Backend episodeKey
        `shouldBe` "https://production-kpbj-storage.sfo3.digitaloceanspaces.com/audio/episodes/2026/10/12/34-parallels_2026-10-12_5d829b7e985e4fd5a40953ea40511104.mp3"

    it "puts the site base URL in front of the media path for local storage" $ do
      absoluteMediaUrl "http://localhost:4000" localBackend episodeKey
        `shouldBe` "http://localhost:4000/media/audio/episodes/2026/10/12/34-parallels_2026-10-12_5d829b7e985e4fd5a40953ea40511104.mp3"

    it "ignores the site base URL for S3 storage" $ hedgehog $ do
      siteBaseUrl <- forAll genSiteBaseUrl
      objectKey <- forAll genObjectKey
      absoluteMediaUrl siteBaseUrl s3Backend objectKey
        === "https://production-kpbj-storage.sfo3.digitaloceanspaces.com/" <> objectKey

    it "gives the site base URL, /media/, and the key for local storage" $ hedgehog $ do
      siteBaseUrl <- forAll genSiteBaseUrl
      objectKey <- forAll genObjectKey
      absoluteMediaUrl siteBaseUrl localBackend objectKey
        === siteBaseUrl <> "/media/" <> objectKey
