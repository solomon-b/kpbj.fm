module API.Dashboard.Analytics.Data.Get.HandlerSpec (spec) where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Data.Get.Handler (mkTopEpisode, rangeToParams, roundTo1)
import API.Dashboard.Analytics.Data.Get.Types (TopEpisode (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Effects.Database.Tables.EpisodePlayEvents qualified as EpisodePlayEvents
import Effects.Database.Tables.ListenerSnapshots (BucketSize (..))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

-- | A fixed reference time for deterministic tests.
refTime :: POSIXTime
refTime = 1700000000

now :: UTCTime
now = posixSecondsToUTCTime refTime

spec :: Spec
spec = describe "API.Dashboard.Analytics.Data.Get.Handler" $ do
  describe "rangeToParams" $ do
    it "24h returns 24 hours ago with FiveMinute buckets" $ do
      let (start, bucket) = rangeToParams "24h" now
      realToFrac (diffUTCTime now start) `shouldBe` (24 * 3600 :: Double)
      bucket `shouldBe` FiveMinute

    it "7d returns 7 days ago with Hourly buckets" $ do
      let (start, bucket) = rangeToParams "7d" now
      realToFrac (diffUTCTime now start) `shouldBe` (7 * 86400 :: Double)
      bucket `shouldBe` Hourly

    it "30d returns 30 days ago with Hourly buckets" $ do
      let (start, bucket) = rangeToParams "30d" now
      realToFrac (diffUTCTime now start) `shouldBe` (30 * 86400 :: Double)
      bucket `shouldBe` Hourly

    it "90d returns 90 days ago with Daily buckets" $ do
      let (start, bucket) = rangeToParams "90d" now
      realToFrac (diffUTCTime now start) `shouldBe` (90 * 86400 :: Double)
      bucket `shouldBe` Daily

    it "unknown range defaults to 7d with Hourly buckets" $ do
      let (start, bucket) = rangeToParams "unknown" now
      realToFrac (diffUTCTime now start) `shouldBe` (7 * 86400 :: Double)
      bucket `shouldBe` Hourly

    it "empty string defaults to 7d" $ do
      let (start, bucket) = rangeToParams "" now
      realToFrac (diffUTCTime now start) `shouldBe` (7 * 86400 :: Double)
      bucket `shouldBe` Hourly

  describe "roundTo1" $ do
    it "rounds 3.14 to 3.1" $ do
      roundTo1 3.14 `shouldBe` 3.1

    it "rounds 3.15 to 3.2" $ do
      roundTo1 3.15 `shouldBe` 3.2

    it "preserves whole numbers" $ do
      roundTo1 5.0 `shouldBe` 5.0

    it "handles zero" $ do
      roundTo1 0.0 `shouldBe` 0.0

    it "handles negative numbers" $ do
      roundTo1 (-2.76) `shouldBe` (-2.8)

    it "result times 10 is an integer (property)" $ hedgehog $ do
      x <- forAll $ Gen.double (Range.linearFrac (-1000) 1000)
      let r = roundTo1 x
          scaled = r * 10
      -- The rounded-then-scaled value should be within epsilon of an integer
      assert $ abs (scaled - fromIntegral (round scaled :: Int)) < 1e-9

  describe "mkTopEpisode" $ do
    it "builds a TopEpisode with formatted title and URL" $ do
      let row = EpisodePlayEvents.TopEpisodeRow { episodeId = 42, showTitle = "Jazz Hour", showSlug = "jazz-hour", episodeNumber = 5, playCount = 100 }
          ep = mkTopEpisode 1 row
      rank ep `shouldBe` 1
      title ep `shouldBe` "Jazz Hour #5"
      showTitle ep `shouldBe` "Jazz Hour"
      plays ep `shouldBe` 100
      url ep `shouldBe` "/shows/jazz-hour/episodes/5"

    it "uses the provided rank, not data from the row" $ do
      let row = EpisodePlayEvents.TopEpisodeRow { episodeId = 1, showTitle = "Show", showSlug = "show", episodeNumber = 1, playCount = 50 }
          ep = mkTopEpisode 3 row
      rank ep `shouldBe` 3

    it "handles episode number 0" $ do
      let row = EpisodePlayEvents.TopEpisodeRow { episodeId = 1, showTitle = "Test Show", showSlug = "test-show", episodeNumber = 0, playCount = 10 }
          ep = mkTopEpisode 1 row
      title ep `shouldBe` "Test Show #0"

    it "handles large play counts" $ do
      let row = EpisodePlayEvents.TopEpisodeRow { episodeId = 1, showTitle = "Popular", showSlug = "popular", episodeNumber = 1, playCount = 999999 }
          ep = mkTopEpisode 1 row
      plays ep `shouldBe` 999999

    it "rank always matches the provided value (property)" $ hedgehog $ do
      n <- forAll $ Gen.int (Range.linear 1 100)
      row <- forAll genTopEpisodeRow
      rank (mkTopEpisode n row) === n

    it "plays is non-negative when playCount is non-negative (property)" $ hedgehog $ do
      n <- forAll $ Gen.int (Range.linear 1 10)
      row <- forAll genTopEpisodeRow
      assert $ plays (mkTopEpisode n row) >= 0

--------------------------------------------------------------------------------
-- Generators

genTopEpisodeRow :: Gen EpisodePlayEvents.TopEpisodeRow
genTopEpisodeRow = do
  eid <- Gen.int64 (Range.linear 1 10000)
  sTitle <- genSlug
  sSlug <- genSlug
  epNum <- Gen.int64 (Range.linear 0 500)
  pCount <- Gen.int64 (Range.linear 0 1000000)
  pure EpisodePlayEvents.TopEpisodeRow
    { EpisodePlayEvents.episodeId = eid
    , EpisodePlayEvents.showTitle = sTitle
    , EpisodePlayEvents.showSlug = sSlug
    , EpisodePlayEvents.episodeNumber = epNum
    , EpisodePlayEvents.playCount = pCount
    }

genSlug :: Gen Text
genSlug = Text.pack <$> Gen.string (Range.linear 1 30) Gen.alphaNum
