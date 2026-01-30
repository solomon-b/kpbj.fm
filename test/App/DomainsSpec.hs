module App.DomainsSpec (spec) where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import App.Domains
import Control.Monad (forM_)
import Data.Text qualified as Text
import Domain.Types.Origin (Origin (..))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------
-- Generators

genEnvironment :: (MonadGen m) => m Environment
genEnvironment = Gen.element [Development, Staging, Production]

genNonDevEnvironment :: (MonadGen m) => m Environment
genNonDevEnvironment = Gen.element [Staging, Production]

--------------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = describe "App.Domains" $ do
  describe "baseDomain" $ do
    it "returns empty for Development" $ do
      baseDomain Development `shouldBe` ""

    it "returns staging.kpbj.fm for Staging" $ do
      baseDomain Staging `shouldBe` "staging.kpbj.fm"

    it "returns kpbj.fm for Production" $ do
      baseDomain Production `shouldBe` "kpbj.fm"

  describe "cookieDomain" $ do
    it "returns empty for Development" $ do
      cookieDomain Development `shouldBe` ""

    it "has leading dot for non-Development (subdomain coverage)" $ hedgehog $ do
      env <- forAll genNonDevEnvironment
      let domain = cookieDomain env
      assert $ "." `Text.isPrefixOf` domain

    it "is derived from baseDomain" $ hedgehog $ do
      env <- forAll genNonDevEnvironment
      let cookie = cookieDomain env
          base = baseDomain env
      cookie === "." <> base

  describe "cookieDomainMaybe" $ do
    it "returns Nothing for Development" $ do
      cookieDomainMaybe Development `shouldBe` Nothing

    it "returns Just for Staging" $ do
      cookieDomainMaybe Staging `shouldBe` Just ".staging.kpbj.fm"

    it "returns Just for Production" $ do
      cookieDomainMaybe Production `shouldBe` Just ".kpbj.fm"

    it "is consistent with cookieDomain for non-Development" $ hedgehog $ do
      env <- forAll genNonDevEnvironment
      cookieDomainMaybe env === Just (cookieDomain env)

  describe "uploadsDomain" $ do
    it "returns empty for Development" $ do
      uploadsDomain Development `shouldBe` ""

    it "has uploads prefix for non-Development" $ hedgehog $ do
      env <- forAll genNonDevEnvironment
      let domain = uploadsDomain env
      assert $ "uploads." `Text.isPrefixOf` domain

    it "is derived from baseDomain" $ hedgehog $ do
      env <- forAll genNonDevEnvironment
      let uploads = uploadsDomain env
          base = baseDomain env
      uploads === "uploads." <> base

  describe "uploadsBaseUrl" $ do
    it "returns empty for Development (relative URLs)" $ do
      uploadsBaseUrl Development `shouldBe` ""

    it "uses HTTPS for non-Development" $ hedgehog $ do
      env <- forAll genNonDevEnvironment
      let url = uploadsBaseUrl env
      assert $ "https://" `Text.isPrefixOf` url

    it "contains the uploads domain" $ hedgehog $ do
      env <- forAll genNonDevEnvironment
      let url = uploadsBaseUrl env
          domain = uploadsDomain env
      assert $ domain `Text.isInfixOf` url

  describe "audioUploadUrl" $ do
    it "ends with /api/uploads/audio" $ hedgehog $ do
      env <- forAll genEnvironment
      let url = audioUploadUrl env
      assert $ "/api/uploads/audio" `Text.isSuffixOf` url

    it "is relative for Development" $ do
      audioUploadUrl Development `shouldBe` "/api/uploads/audio"

    it "is absolute for non-Development" $ hedgehog $ do
      env <- forAll genNonDevEnvironment
      let url = audioUploadUrl env
      assert $ "https://" `Text.isPrefixOf` url

  describe "Cross-domain consistency" $ do
    it "cookie domain covers uploads domain for Staging" $ do
      let cookie = cookieDomain Staging
          uploads = uploadsDomain Staging
      -- .staging.kpbj.fm should cover uploads.staging.kpbj.fm
      cookie `shouldBe` ".staging.kpbj.fm"
      uploads `shouldBe` "uploads.staging.kpbj.fm"
      -- Verify the relationship: uploads ends with cookie domain (minus leading dot)
      uploads `shouldSatisfy` Text.isSuffixOf (Text.drop 1 cookie)

    it "cookie domain covers uploads domain for Production" $ do
      let cookie = cookieDomain Production
          uploads = uploadsDomain Production
      cookie `shouldBe` ".kpbj.fm"
      uploads `shouldBe` "uploads.kpbj.fm"
      uploads `shouldSatisfy` Text.isSuffixOf (Text.drop 1 cookie)

    it "cookie domain covers uploads domain (property)" $ hedgehog $ do
      env <- forAll genNonDevEnvironment
      let cookie = cookieDomain env
          uploads = uploadsDomain env
      -- uploads.X.kpbj.fm should end with X.kpbj.fm (cookie domain minus dot)
      assert $ Text.isSuffixOf (Text.drop 1 cookie) uploads

  describe "allowedOrigins" $ do
    it "includes main site for Staging" $ do
      allowedOrigins Staging `shouldSatisfy` elem "https://staging.kpbj.fm"

    it "includes main site for Production" $ do
      allowedOrigins Production `shouldSatisfy` elem "https://www.kpbj.fm"

    it "includes apex domain for Production" $ do
      allowedOrigins Production `shouldSatisfy` elem "https://kpbj.fm"

    it "includes uploads subdomain" $ hedgehog $ do
      env <- forAll genNonDevEnvironment
      let origins = allowedOrigins env
          uploadsUrl = Origin $ "https://" <> uploadsDomain env
      assert $ uploadsUrl `elem` origins

    it "rejects evil origins (not in list)" $ do
      let evilOrigins = ["https://evil.com", "https://kpbj.fm.evil.com", "http://kpbj.fm"]
      forM_ [Development, Staging, Production] $ \env ->
        forM_ evilOrigins $ \evil ->
          allowedOrigins env `shouldNotSatisfy` elem evil
