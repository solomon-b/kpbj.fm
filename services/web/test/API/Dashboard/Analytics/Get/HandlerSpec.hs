module API.Dashboard.Analytics.Get.HandlerSpec (spec) where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Get.Handler (mkHeatmap, mkNewsletter, mkTopDimension, mkTopEpisode, mkTopShow, rangeToParams, roundTo1)
import API.Dashboard.Analytics.Get.Types (AnalyticsRange (..), ChartSeries (..), Heatmap (..), NewsletterData (..), TopDimensionEntry (..), TopEpisode (..), TopShowEntry (..), heatmapLevel)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime (..), diffUTCTime, fromGregorian)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Effects.Database.Tables.EpisodePlayEvents qualified as EpisodePlayEvents
import Effects.Database.Tables.GaSnapshots qualified as GaSnapshots
import Effects.Database.Tables.ListenerSnapshots (BucketSize (..))
import Effects.Database.Tables.ListenerSnapshots qualified as ListenerSnapshots
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

-- | A time on the hour in UTC.
utcAt :: Integer -> Int -> Int -> Int -> UTCTime
utcAt y m d h = UTCTime (fromGregorian y m d) (fromIntegral (h * 3600))

spec :: Spec
spec = describe "API.Dashboard.Analytics.Get.Handler" $ do
  describe "rangeToParams" $ do
    it "24h returns 24 hours ago with FiveMinute buckets" $ do
      let (start, bucket) = rangeToParams Range24h now
      realToFrac (diffUTCTime now start) `shouldBe` (24 * 3600 :: Double)
      bucket `shouldBe` FiveMinute

    -- now is 14:13 PST on 2023-11-14. DST ended on 2023-11-05.
    it "7d starts at midnight PST 6 days before today with Hourly buckets" $ do
      let (start, bucket) = rangeToParams Range7d now
      start `shouldBe` utcAt 2023 11 8 8
      bucket `shouldBe` Hourly

    it "30d starts at midnight PDT 29 days before today with Hourly buckets" $ do
      let (start, bucket) = rangeToParams Range30d now
      start `shouldBe` utcAt 2023 10 16 7
      bucket `shouldBe` Hourly

    it "90d starts at midnight PDT 89 days before today with Daily buckets" $ do
      let (start, bucket) = rangeToParams Range90d now
      start `shouldBe` utcAt 2023 8 17 7
      bucket `shouldBe` Daily

    it "all starts before the first analytics data with Daily buckets" $ do
      let (start, bucket) = rangeToParams RangeAll now
      start `shouldSatisfy` (< UTCTime (fromGregorian 2026 3 15) 0)
      bucket `shouldBe` Daily

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
      let row = EpisodePlayEvents.TopEpisodeRow {episodeId = 42, showTitle = "Jazz Hour", showSlug = "jazz-hour", episodeNumber = 5, playCount = 100}
          ep = mkTopEpisode 1 row
      ep.rank `shouldBe` 1
      ep.title `shouldBe` "Jazz Hour #5"
      ep.showTitle `shouldBe` "Jazz Hour"
      ep.plays `shouldBe` 100
      ep.url `shouldBe` "/shows/jazz-hour/episodes/5"

    it "uses the provided rank, not data from the row" $ do
      let row = EpisodePlayEvents.TopEpisodeRow {episodeId = 1, showTitle = "Show", showSlug = "show", episodeNumber = 1, playCount = 50}
          ep = mkTopEpisode 3 row
      ep.rank `shouldBe` 3

    it "handles episode number 0" $ do
      let row = EpisodePlayEvents.TopEpisodeRow {episodeId = 1, showTitle = "Test Show", showSlug = "test-show", episodeNumber = 0, playCount = 10}
          ep = mkTopEpisode 1 row
      ep.title `shouldBe` "Test Show #0"

    it "handles large play counts" $ do
      let row = EpisodePlayEvents.TopEpisodeRow {episodeId = 1, showTitle = "Popular", showSlug = "popular", episodeNumber = 1, playCount = 999999}
          ep = mkTopEpisode 1 row
      ep.plays `shouldBe` 999999

    it "rank always matches the provided value (property)" $ hedgehog $ do
      n <- forAll $ Gen.int (Range.linear 1 100)
      row <- forAll genTopEpisodeRow
      (mkTopEpisode n row).rank === n

    it "plays is non-negative when playCount is non-negative (property)" $ hedgehog $ do
      n <- forAll $ Gen.int (Range.linear 1 10)
      row <- forAll genTopEpisodeRow
      assert $ (mkTopEpisode n row).plays >= 0

  describe "mkTopDimension" $ do
    it "builds a TopDimensionEntry preserving label and sessions" $ do
      let row = GaSnapshots.TopDimensionRow {value = "google", sessions = 1234}
          entry = mkTopDimension 1 row
      entry.rank `shouldBe` 1
      entry.label `shouldBe` "google"
      (entry.sessions :: Int) `shouldBe` 1234

    it "uses the provided rank, not data from the row" $ do
      let row = GaSnapshots.TopDimensionRow {value = "(direct)", sessions = 50}
          entry = mkTopDimension 7 row
      entry.rank `shouldBe` 7

    it "rank always matches the provided value (property)" $ hedgehog $ do
      n <- forAll $ Gen.int (Range.linear 1 50)
      row <- forAll genTopDimensionRow
      (mkTopDimension n row).rank === n

    it "preserves the label exactly (property)" $ hedgehog $ do
      n <- forAll $ Gen.int (Range.linear 1 50)
      row <- forAll genTopDimensionRow
      (mkTopDimension n row).label === row.value

  describe "mkHeatmap" $ do
    it "has 7 rows of 24 empty cells with no data" $
      (mkHeatmap []).cells `shouldBe` replicate 7 (replicate 24 Nothing)

    it "puts Monday 19:00 in row 0, column 19" $ do
      let cells = (mkHeatmap [ListenerSnapshots.HeatmapRow 1 19 5]).cells
      (cells !! 0 !! 19) `shouldBe` Just 5
      length (filter (/= Nothing) (concat cells)) `shouldBe` 1

    it "puts Sunday 00:00 in row 6, column 0" $
      ((mkHeatmap [ListenerSnapshots.HeatmapRow 7 0 2]).cells !! 6 !! 0) `shouldBe` Just 2

  describe "heatmapLevel" $ do
    it "is 0 when the maximum is 0" $
      heatmapLevel 0 0 `shouldBe` 0

    it "is 4 at the maximum" $
      heatmapLevel 5 5 `shouldBe` 4

    it "is 0 for a value of 0" $
      heatmapLevel 0 5 `shouldBe` 0

    it "is between 0 and 4 for a value between 0 and the maximum (property)" $ hedgehog $ do
      m <- forAll $ Gen.double (Range.linearFrac 0.1 500)
      v <- forAll $ Gen.double (Range.linearFrac 0 m)
      assert $ heatmapLevel v m >= 0 && heatmapLevel v m <= 4

  describe "mkNewsletter" $ do
    it "has one point for each day, with the running total" $ do
      let nl = mkNewsletter (fromGregorian 2026 6 10) (fromGregorian 2026 6 12) 10 [(2, "2026-06-10"), (1, "2026-06-12")]
      nl.series.labels `shouldBe` ["2026-06-10", "2026-06-11", "2026-06-12"]
      nl.series.values `shouldBe` [12, 12, 13]
      nl.total `shouldBe` 13
      nl.new `shouldBe` 3

    it "draws a flat line at the total before the range when the range has no signups" $ do
      let nl = mkNewsletter (fromGregorian 2026 6 10) (fromGregorian 2026 6 11) 10 []
      nl.series.labels `shouldBe` ["2026-06-10", "2026-06-11"]
      nl.series.values `shouldBe` [10, 10]
      nl.total `shouldBe` 10
      nl.new `shouldBe` 0

  describe "mkTopShow" $
    it "builds a TopShowEntry with the rank and the show URL" $ do
      let entry = mkTopShow 2 EpisodePlayEvents.TopShowRow {showTitle = "Jazz Hour", showSlug = "jazz-hour", playCount = 40}
      entry.rank `shouldBe` 2
      entry.title `shouldBe` "Jazz Hour"
      entry.plays `shouldBe` 40
      entry.url `shouldBe` "/shows/jazz-hour"

--------------------------------------------------------------------------------
-- Generators

genTopEpisodeRow :: Gen EpisodePlayEvents.TopEpisodeRow
genTopEpisodeRow = do
  eid <- Gen.int64 (Range.linear 1 10000)
  sTitle <- genSlug
  sSlug <- genSlug
  epNum <- Gen.int64 (Range.linear 0 500)
  pCount <- Gen.int64 (Range.linear 0 1000000)
  pure
    EpisodePlayEvents.TopEpisodeRow
      { EpisodePlayEvents.episodeId = eid,
        EpisodePlayEvents.showTitle = sTitle,
        EpisodePlayEvents.showSlug = sSlug,
        EpisodePlayEvents.episodeNumber = epNum,
        EpisodePlayEvents.playCount = pCount
      }

genSlug :: Gen Text
genSlug = Text.pack <$> Gen.string (Range.linear 1 30) Gen.alphaNum

genTopDimensionRow :: Gen GaSnapshots.TopDimensionRow
genTopDimensionRow = do
  v <- genSlug
  s <- Gen.int64 (Range.linear 0 1000000)
  pure
    GaSnapshots.TopDimensionRow
      { GaSnapshots.value = v,
        GaSnapshots.sessions = s
      }
