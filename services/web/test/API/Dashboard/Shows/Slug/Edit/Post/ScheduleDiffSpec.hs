-- | Pure unit tests for schedule diff logic.
--
-- Tests that schedulesMatch correctly detects when form schedules match
-- the existing DB templates, preventing unnecessary terminate-and-recreate cycles.
-- Also includes property tests for the set-difference algebra used by
-- slot-level diffing in updateScheduleTemplates.
module API.Dashboard.Shows.Slug.Edit.Post.ScheduleDiffSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Post.Handler (ParsedScheduleSlot (..), normalizeTemplate, parseScheduleSlot, schedulesMatch, validateNoOverlaps)
import API.Dashboard.Shows.Slug.Edit.Post.Route (ScheduleSlotInfo (..))
import Data.Either (isLeft)
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
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "API.Dashboard.Shows.Slug.Edit.Post.Handler schedule diff" $ do
    describe "normalizeTemplate" $ do
      it "extracts and sorts fields from a DB template" $ do
        let template = mkTemplate (Just Friday) (Just [3, 1, 2]) (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)
        normalizeTemplate template
          `shouldBe` Just (ParsedScheduleSlot Friday [1, 2, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) Nothing)

      it "handles Nothing weeks as all weeks (weekly)" $ do
        let template = mkTemplate (Just Monday) Nothing (TimeOfDay 14 0 0) (TimeOfDay 16 0 0)
        normalizeTemplate template
          `shouldBe` Just (ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 14 0 0) (TimeOfDay 16 0 0) Nothing)

      it "returns Nothing for template with no day of week" $ do
        let template = mkTemplate Nothing (Just [1, 2]) (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)
        normalizeTemplate template `shouldBe` Nothing

    describe "parseScheduleSlot" $ do
      it "parses and normalizes a valid form slot" $ do
        let slot = mkSlot "friday" [3, 1, 2] "08:00" 120
        parseScheduleSlot slot
          `shouldBe` Right (ParsedScheduleSlot Friday [1, 2, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) Nothing)

      it "rejects invalid start time" $ do
        let slot = mkSlot "friday" [1, 2] "invalid" 60
        parseScheduleSlot slot `shouldBe` Left "Invalid start time: invalid"

      it "rejects invalid duration" $ do
        let slot = mkSlot "friday" [1, 2] "08:00" 45
        parseScheduleSlot slot `shouldBe` Left "Invalid duration: 45 (must be 30, 60, or 120)"

      it "rejects unknown day of week" $ do
        let slot = mkSlot "funday" [1] "08:00" 60
        parseScheduleSlot slot `shouldBe` Left "Invalid day of week: funday"

    describe "schedulesMatch" $ do
      it "returns True for identical schedules" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing]
        schedulesMatch templates slots `shouldBe` True

      it "returns True for reordered schedules (set comparison)" $ do
        let templates =
              [ mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0),
                mkTemplate (Just Monday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)
              ]
            slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) Nothing,
                ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
              ]
        schedulesMatch templates slots `shouldBe` True

      it "returns True when DB template has unsorted weeks (normalizeTemplate sorts)" $ do
        let templates = [mkTemplate (Just Friday) (Just [5, 1, 3]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Friday [1, 3, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing]
        schedulesMatch templates slots `shouldBe` True

      it "returns False for different day of week" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Saturday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing]
        schedulesMatch templates slots `shouldBe` False

      it "returns False for different weeks of month" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Friday [1, 3] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing]
        schedulesMatch templates slots `shouldBe` False

      it "returns False for different start time" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 20 0 0) (TimeOfDay 21 0 0) Nothing]
        schedulesMatch templates slots `shouldBe` False

      it "returns False for different end time" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 22 0 0) Nothing]
        schedulesMatch templates slots `shouldBe` False

      it "returns False for added schedule slot" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots =
              [ ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing,
                ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) Nothing
              ]
        schedulesMatch templates slots `shouldBe` False

      it "returns False for removed schedule slot" $ do
        let templates =
              [ mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0),
                mkTemplate (Just Monday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)
              ]
            slots = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing]
        schedulesMatch templates slots `shouldBe` False

      it "returns True when DB has NULL weeks (weekly) and form sends [1..5]" $ do
        let templates = [mkTemplate (Just Friday) Nothing (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slots = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing]
        schedulesMatch templates slots `shouldBe` True

      it "returns True for empty schedules" $ do
        schedulesMatch [] [] `shouldBe` True

      it "returns False when DB has schedules but form is empty" $ do
        let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
        schedulesMatch templates [] `shouldBe` False

      it "returns False when form has schedules but DB is empty" $ do
        let slots = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing]
        schedulesMatch [] slots `shouldBe` False

    describe "validateNoOverlaps" $ do
      -- No-overlap cases
      it "allows slots on different days" $ do
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing,
                ParsedScheduleSlot Tuesday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldBe` Right slots

      it "allows same day with non-overlapping weeks" $ do
        let slots =
              [ ParsedScheduleSlot Monday [1] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing,
                ParsedScheduleSlot Monday [3] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldBe` Right slots

      it "allows adjacent time slots (end of first = start of second)" $ do
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing,
                ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 12 0 0) (TimeOfDay 14 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldBe` Right slots

      it "allows two slots without replays that don't overlap" $ do
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing,
                ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 14 0 0) (TimeOfDay 16 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldBe` Right slots

      -- Primary vs primary overlap
      it "rejects overlapping primary slots on the same day and weeks" $ do
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing,
                ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 11 0 0) (TimeOfDay 13 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldSatisfy` isLeft

      -- Replay overlaps own primary
      it "rejects a slot whose replay overlaps its own primary" $ do
        -- 10:00-12:00 primary with replay at 11:00 → replay 11:00-13:00 overlaps primary
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) (Just (TimeOfDay 11 0 0))
              ]
        validateNoOverlaps slots `shouldSatisfy` isLeft

      -- Replay overlaps another slot's primary
      it "rejects when a slot's replay overlaps another slot's primary" $ do
        -- Slot A: 10:00-12:00, replay at 14:00 → replay 14:00-16:00
        -- Slot B: 15:00-17:00 primary → overlaps slot A's replay
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) (Just (TimeOfDay 14 0 0)),
                ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 15 0 0) (TimeOfDay 17 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldSatisfy` isLeft

      -- Primary overlaps another slot's replay
      it "rejects when a slot's primary overlaps another slot's replay" $ do
        -- Slot A: 14:00-16:00 primary, replay at 10:00 → replay 10:00-12:00
        -- Slot B: 11:00-12:00 primary → overlaps slot A's replay
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 14 0 0) (TimeOfDay 16 0 0) (Just (TimeOfDay 10 0 0)),
                ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 11 0 0) (TimeOfDay 12 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldSatisfy` isLeft

      -- Replay vs replay overlap
      it "rejects when two slots' replays overlap each other" $ do
        -- Slot A: 10:00-12:00 (2h), replay at 18:00 → replay 18:00-20:00
        -- Slot B: 14:00-16:00 (2h), replay at 19:00 → replay 19:00-21:00
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) (Just (TimeOfDay 18 0 0)),
                ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 14 0 0) (TimeOfDay 16 0 0) (Just (TimeOfDay 19 0 0))
              ]
        validateNoOverlaps slots `shouldSatisfy` isLeft

      -- Midnight-ending show cases
      it "allows midnight-ending show with replay starting at midnight" $ do
        -- 23:00-00:00 (1h) with replay at 00:00 → replay 00:00-01:00
        -- Primary is overnight (end 00:00 <= start 23:00), replay is standard
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 23 0 0) (TimeOfDay 0 0 0) (Just (TimeOfDay 0 0 0))
              ]
        validateNoOverlaps slots `shouldBe` Right slots

      it "rejects midnight-ending show with replay during primary" $ do
        -- 23:00-00:00 (1h) with replay at 23:30 → replay 23:30-00:30
        -- Both overnight → always overlap
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 23 0 0) (TimeOfDay 0 0 0) (Just (TimeOfDay 23 30 0))
              ]
        validateNoOverlaps slots `shouldSatisfy` isLeft

      -- Overnight primary with non-overlapping replay
      it "allows overnight show with non-overlapping replay" $ do
        -- 23:00-01:00 (2h overnight) with replay at 02:00 → replay 02:00-04:00
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 23 0 0) (TimeOfDay 1 0 0) (Just (TimeOfDay 2 0 0))
              ]
        validateNoOverlaps slots `shouldBe` Right slots

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
      stReplayStartTime = Nothing
    }

-- | Create a ScheduleSlotInfo for testing.
mkSlot :: Text -> [Int64] -> Text -> Int -> ScheduleSlotInfo
mkSlot dow weeks start dur =
  ScheduleSlotInfo
    { dayOfWeek = dow,
      weeksOfMonth = weeks,
      startTime = start,
      duration = dur,
      replayTime = Nothing
    }

--------------------------------------------------------------------------------
-- Generators

genParsedScheduleSlot :: Gen ParsedScheduleSlot
genParsedScheduleSlot = do
  day <- Gen.element [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
  weeks <- sort <$> Gen.subsequence [1, 2, 3, 4, 5]
  startHour <- Gen.int (Range.linear 6 22)
  let endHour = min 23 (startHour + 2)
  mReplay <- Gen.maybe $ do
    replayHour <- Gen.int (Range.linear endHour 23)
    pure $ TimeOfDay replayHour 0 0
  pure $
    ParsedScheduleSlot
      { pssDay = day,
        pssWeeks = weeks,
        pssStart = TimeOfDay startHour 0 0,
        pssEnd = TimeOfDay endHour 0 0,
        pssReplayStartTime = mReplay
      }
