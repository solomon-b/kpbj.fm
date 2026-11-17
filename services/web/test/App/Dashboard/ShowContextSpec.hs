module App.Dashboard.ShowContextSpec (spec) where

--------------------------------------------------------------------------------

import App.Dashboard.ShowContext (ShowSection (..), lookupCurrentShowSlug, pickCurrentShow)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Tables.Shows qualified as Shows
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Servant (parseUrlPiece, toUrlPiece)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "App.Dashboard.ShowContext" $ do
    describe "pickCurrentShow" $ do
      it "returns the show that the slug names" $
        pick (Just "b") ["a", "b", "c"] `shouldBe` Just "b"
      it "returns the first show when the slug is outside the list" $
        pick (Just "gone") ["a", "b"] `shouldBe` Just "a"
      it "returns the first show when no slug is given" $
        pick Nothing ["a", "b"] `shouldBe` Just "a"
      it "returns nothing for an empty list" $
        pick (Just "a") [] `shouldBe` Nothing
      it "always returns a member of the list" $ hedgehog $ do
        mSlug <- forAll $ Gen.maybe genSlug
        slugs <- forAll $ Gen.list (Range.linear 0 8) genSlug
        let result = pick mSlug slugs
        assert $ maybe (null slugs) (`elem` slugs) result

    describe "lookupCurrentShowSlug" $ do
      it "finds the slug among several cookies" $
        lookupCurrentShowSlug (Cookie "session-id-production=abc; dashboard_show=morning-show; _flash=x")
          `shouldBe` Just (Slug "morning-show")
      it "returns nothing when the cookie is absent" $
        lookupCurrentShowSlug (Cookie "_flash=x") `shouldBe` Nothing
      it "returns nothing for an empty value" $
        lookupCurrentShowSlug (Cookie "dashboard_show=") `shouldBe` Nothing
      it "decodes a percent-encoded value" $
        lookupCurrentShowSlug (Cookie "dashboard_show=caf%C3%A9") `shouldBe` Just (Slug "café")

    describe "ShowSection" $ do
      it "round-trips through the URL piece" $
        for_ [minBound .. maxBound] $ \section ->
          parseUrlPiece (toUrlPiece section) `shouldBe` Right (section :: ShowSection)
      it "rejects an unknown section" $
        (parseUrlPiece "bogus" :: Either Text ShowSection) `shouldSatisfy` isLeft

--------------------------------------------------------------------------------

-- | Apply 'pickCurrentShow' to shows that have the given slugs.
pick :: Maybe Slug -> [Slug] -> Maybe Slug
pick mSlug slugs = (.slug) <$> pickCurrentShow mSlug (zipWith mkShow [1 ..] slugs)

genSlug :: Gen Slug
genSlug = Slug <$> Gen.element ["a", "b", "c", "d", "e"]

mkShow :: Int64 -> Slug -> Shows.Model
mkShow showId slug =
  Shows.Show
    { Shows.id = Shows.Id showId,
      Shows.title = "Show",
      Shows.slug = slug,
      Shows.description = Nothing,
      Shows.logoUrl = Nothing,
      Shows.status = Shows.Active,
      Shows.createdAt = epoch,
      Shows.updatedAt = epoch,
      Shows.deletedAt = Nothing
    }
  where
    epoch = UTCTime (fromGregorian 2026 1 1) 0
