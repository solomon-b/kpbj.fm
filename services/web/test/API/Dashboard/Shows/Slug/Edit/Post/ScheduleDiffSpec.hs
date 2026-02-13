-- | Pure unit tests for schedule diff logic.
--
-- Tests that schedulesMatch correctly detects when form schedules match
-- the existing DB templates, preventing unnecessary terminate-and-recreate cycles.
-- Also includes property tests for the set-difference algebra used by
-- slot-level diffing in updateScheduleTemplates.
module API.Dashboard.Shows.Slug.Edit.Post.ScheduleDiffSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Post.Handler (ParsedScheduleSlot (..), normalizeTemplate, parseScheduleSlot, schedulesMatch)
import API.Dashboard.Shows.Slug.Edit.Post.Route (ScheduleSlotInfo (..))
import Data.Int (Int64)
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (DayOfWeek (..), TimeOfDay (..), UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rel8 qualified
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "API.Dashboard.Shows.Slug.Edit.Post.Handler schedule diff" $ do
    describe "normalizeTemplate" $ do
      it "extracts and sorts fields from a DB template" $ do
        let template = mkTemplate (Just Friday) (Just [3, 1, 2]) (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)
        normalizeTemplate template
          `shouldBe` Just (ParsedScheduleSlot Friday [1, 2, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0))

      it "handles Nothing weeks as empty list" $ do
        let template = mkTemplate (Just Monday) Nothing (TimeOfDay 14 0 0) (TimeOfDay 16 0 0)
        normalizeTemplate template
          `shouldBe` Just (ParsedScheduleSlot Monday [] (TimeOfDay 14 0 0) (TimeOfDay 16 0 0))

      it "returns Nothing for template with no day of week" $ do
        let template = mkTemplate Nothing (Just [1, 2]) (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)
        normalizeTemplate template `shouldBe` Nothing

    describe "parseScheduleSlot" $ do
      it "parses and normalizes a valid form slot" $ do
        let slot = mkSlot "friday" [3, 1, 2] "08:00" "10:00"
        parseScheduleSlot slot
          `shouldBe` Right (ParsedScheduleSlot Friday [1, 2, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0))

      it "rejects invalid start time" $ do
        let slot = mkSlot "friday" [1, 2] "invalid" "10:00"
        parseScheduleSlot slot `shouldBe` Left "Invalid start time: invalid"

      it "rejects invalid end time" $ do
        let slot = mkSlot "friday" [1, 2] "08:00" "invalid"
        parseScheduleSlot slot `shouldBe` Left "Invalid end time: invalid"

      it "rejects unknown day of week" $ do
        let slot = mkSlot "funday" [1] "08:00" "10:00"
        parseScheduleSlot slot `shouldBe` Left "Invalid day of week: funday"

    describe "schedulesMatch" $ do
      it "returns True for identical schedules" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
        schedulesMatch templates slots `shouldBe` True

      it "returns True for reordered schedules (set comparison)" $ do
        let templates =
              [ mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0),
                mkTemplate (Just Monday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)
              ]
            slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0),
                ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)
              ]
        schedulesMatch templates slots `shouldBe` True

      it "returns True when DB template has unsorted weeks (normalizeTemplate sorts)" $ do
        let templates = [mkTemplate (Just Friday) (Just [5, 1, 3]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Friday [1, 3, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
        schedulesMatch templates slots `shouldBe` True

      it "returns False for different day of week" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Saturday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
        schedulesMatch templates slots `shouldBe` False

      it "returns False for different weeks of month" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Friday [1, 3] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
        schedulesMatch templates slots `shouldBe` False

      it "returns False for different start time" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 20 0 0) (TimeOfDay 21 0 0)]
        schedulesMatch templates slots `shouldBe` False

      it "returns False for different end time" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 22 0 0)]
        schedulesMatch templates slots `shouldBe` False

      it "returns False for added schedule slot" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots =
              [ ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0),
                ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)
              ]
        schedulesMatch templates slots `shouldBe` False

      it "returns False for removed schedule slot" $ do
        let templates =
              [ mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0),
                mkTemplate (Just Monday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)
              ]
            slots = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
        schedulesMatch templates slots `shouldBe` False

      it "returns True for empty schedules" $ do
        schedulesMatch [] [] `shouldBe` True

      it "returns False when DB has schedules but form is empty" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
        schedulesMatch templates [] `shouldBe` False

      it "returns False when form has schedules but DB is empty" $ do
        let slots = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
        schedulesMatch [] slots `shouldBe` False

    describe "diff algebra" $ do
      it "unchanged slots appear in neither removed nor added" $ hedgehog $ do
        db <- forAll $ Gen.list (Range.linear 0 10) genParsedScheduleSlot
        form <- forAll $ Gen.list (Range.linear 0 10) genParsedScheduleSlot
        let dbSet = Set.fromList db
            formSet = Set.fromList form
            removed = Set.difference dbSet formSet
            added = Set.difference formSet dbSet
            unchanged = Set.intersection dbSet formSet
        Set.intersection unchanged removed === Set.empty
        Set.intersection unchanged added === Set.empty

      it "removed + unchanged = db, added + unchanged = form" $ hedgehog $ do
        db <- forAll $ Gen.list (Range.linear 0 10) genParsedScheduleSlot
        form <- forAll $ Gen.list (Range.linear 0 10) genParsedScheduleSlot
        let dbSet = Set.fromList db
            formSet = Set.fromList form
            removed = Set.difference dbSet formSet
            added = Set.difference formSet dbSet
            unchanged = Set.intersection dbSet formSet
        Set.union unchanged removed === dbSet
        Set.union unchanged added === formSet

      it "identical sets produce no changes" $ hedgehog $ do
        slots <- forAll $ Gen.list (Range.linear 0 10) genParsedScheduleSlot
        let s = Set.fromList slots
        Set.difference s s === Set.empty

--------------------------------------------------------------------------------
-- Test Helpers

-- | Create a ScheduleTemplate Result for testing.
mkTemplate :: Maybe DayOfWeek -> Maybe [Int64] -> TimeOfDay -> TimeOfDay -> ShowSchedule.ScheduleTemplate Rel8.Result
mkTemplate dow weeks start end =
  ShowSchedule.ScheduleTemplate
    { stId = ShowSchedule.TemplateId 1,
      stShowId = Shows.Id 1,
      stDayOfWeek = dow,
      stWeeksOfMonth = weeks,
      stStartTime = start,
      stEndTime = end,
      stTimezone = "America/Los_Angeles",
      stCreatedAt = UTCTime (fromGregorian 2025 1 1) 0,
      stAirsTwiceDaily = False
    }

-- | Create a ScheduleSlotInfo for testing.
mkSlot :: Text -> [Int64] -> Text -> Text -> ScheduleSlotInfo
mkSlot dow weeks start end =
  ScheduleSlotInfo
    { dayOfWeek = dow,
      weeksOfMonth = weeks,
      startTime = start,
      endTime = end
    }

--------------------------------------------------------------------------------
-- Generators

genParsedScheduleSlot :: Gen ParsedScheduleSlot
genParsedScheduleSlot = do
  day <- Gen.element [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
  weeks <- sort <$> Gen.subsequence [1, 2, 3, 4, 5]
  startHour <- Gen.int (Range.linear 6 22)
  let endHour = min 23 (startHour + 2)
  pure $
    ParsedScheduleSlot
      { pssDay = day,
        pssWeeks = weeks,
        pssStart = TimeOfDay startHour 0 0,
        pssEnd = TimeOfDay endHour 0 0
      }
