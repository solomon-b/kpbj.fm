-- | Pure unit tests for schedule diff logic.
--
-- Tests that schedulesMatch correctly detects when form schedules match
-- the existing DB templates, preventing unnecessary terminate-and-recreate cycles.
-- Also includes property tests for the set-difference algebra used by
-- slot-level diffing in updateScheduleTemplates.
module API.Dashboard.Shows.Slug.Edit.Post.ScheduleDiffSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Post.Handler (ParsedScheduleSlot (..), normalizeTemplate, parseScheduleSlot, removedTemplates, scheduleUpdateFlash, schedulesMatch, validateNoOverlaps)
import API.Dashboard.Shows.Slug.Edit.Post.Route (ScheduleSlotInfo (..))
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..))
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (Day, DayOfWeek (..), TimeOfDay (..), UTCTime (..), addDays, diffDays)
import Data.Time qualified as Time
import Data.Time.Calendar (fromGregorian, toGregorian)
import Domain.Types.Timezone (addMinutesToTimeOfDay, slotDurationMins)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rel8 qualified
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog)
import Test.Hspec.QuickCheck (modifyMaxSuccess)

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

    describe "removedTemplates" $ do
      it "returns a template whose slot was removed from the form" $ do
        let friday = mkTemplateWith (ShowSchedule.TemplateId 1) (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
            monday = mkTemplateWith (ShowSchedule.TemplateId 2) (Just Monday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) Nothing
            -- The form keeps only the Friday slot; the Monday slot is removed.
            form = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing]
        map ShowSchedule.stId (removedTemplates [friday, monday] form) `shouldBe` [ShowSchedule.TemplateId 2]

      it "returns the original template when a slot is re-keyed by changing weeks" $ do
        let template = mkTemplateWith (ShowSchedule.TemplateId 5) (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
            -- Same day and time, but the weeks changed, so the signature no longer matches.
            form = [ParsedScheduleSlot Friday [1, 3] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing]
        map ShowSchedule.stId (removedTemplates [template] form) `shouldBe` [ShowSchedule.TemplateId 5]

      it "returns the original template when a slot is re-keyed by changing replay" $ do
        let template = mkTemplateWith (ShowSchedule.TemplateId 7) (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) (Just (TimeOfDay 2 0 0))
            -- Same day, weeks, and time, but the replay time changed.
            form = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) (Just (TimeOfDay 3 0 0))]
        map ShowSchedule.stId (removedTemplates [template] form) `shouldBe` [ShowSchedule.TemplateId 7]

      it "does not return a template whose slot is unchanged" $ do
        let template = mkTemplateWith (ShowSchedule.TemplateId 1) (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
            form = [ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing]
        map ShowSchedule.stId (removedTemplates [template] form) `shouldBe` []

      it "ignores slots added in the form that have no DB template" $ do
        let template = mkTemplateWith (ShowSchedule.TemplateId 1) (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
            -- The form keeps the Friday slot and adds a brand-new Monday slot.
            form =
              [ ParsedScheduleSlot Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing,
                ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) Nothing
              ]
        map ShowSchedule.stId (removedTemplates [template] form) `shouldBe` []

    describe "scheduleUpdateFlash" $ do
      it "empty list yields a Success flash" $
        case scheduleUpdateFlash [] of
          FlashMessage t _ _ -> t `shouldBe` Success

      it "non-empty list yields a Warning flash" $ do
        let ref =
              Episodes.UpcomingEpisodeRef
                (Episodes.Id 1)
                (Episodes.EpisodeNumber 3)
                (UTCTime (fromGregorian 2026 8 1) 0)
        case scheduleUpdateFlash [ref] of
          FlashMessage t _ _ -> t `shouldBe` Warning

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

      -- Slots that cross midnight onto the next day
      it "rejects a slot whose tail takes the next day's slot" $ do
        -- Monday 23:00-01:00 occupies Tuesday 00:00-01:00.
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 23 0 0) (TimeOfDay 1 0 0) Nothing,
                ParsedScheduleSlot Tuesday [1, 2, 3, 4, 5] (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldSatisfy` isLeft

      it "rejects the same pair in the other order" $ do
        let slots =
              [ ParsedScheduleSlot Tuesday [1, 2, 3, 4, 5] (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) Nothing,
                ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 23 0 0) (TimeOfDay 1 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldSatisfy` isLeft

      it "rejects a Saturday tail that lands on Sunday" $ do
        let slots =
              [ ParsedScheduleSlot Saturday [1, 2, 3, 4, 5] (TimeOfDay 23 0 0) (TimeOfDay 1 0 0) Nothing,
                ParsedScheduleSlot Sunday [1, 2, 3, 4, 5] (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldSatisfy` isLeft

      it "allows a next-day slot that starts where the tail ends" $ do
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 23 0 0) (TimeOfDay 1 0 0) Nothing,
                ParsedScheduleSlot Tuesday [1, 2, 3, 4, 5] (TimeOfDay 1 0 0) (TimeOfDay 2 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldBe` Right slots

      it "allows a slot that stops at midnight beside the next day's slot" $ do
        -- Monday 23:00-00:00 stops at midnight and takes nothing from Tuesday.
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 23 0 0) (TimeOfDay 0 0 0) Nothing,
                ParsedScheduleSlot Tuesday [1, 2, 3, 4, 5] (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldBe` Right slots

      it "rejects a replay whose tail takes the next day's slot" $ do
        -- Monday 22:00-23:00 (1h) with a replay at 23:30 → replay 23:30-00:30.
        let slots =
              [ ParsedScheduleSlot Monday [1, 2, 3, 4, 5] (TimeOfDay 22 0 0) (TimeOfDay 23 0 0) (Just (TimeOfDay 23 30 0)),
                ParsedScheduleSlot Tuesday [1, 2, 3, 4, 5] (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldSatisfy` isLeft

      -- The week of the month across midnight
      it "rejects a week-1 tail that lands in week 2" $ do
        -- The first Monday can be the 7th, so the next day is in week 2.
        let slots =
              [ ParsedScheduleSlot Monday [1] (TimeOfDay 23 0 0) (TimeOfDay 1 0 0) Nothing,
                ParsedScheduleSlot Tuesday [2] (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldSatisfy` isLeft

      it "rejects a week-5 tail that lands in week 1" $ do
        -- A week-5 Monday can be the last day of a month.
        let slots =
              [ ParsedScheduleSlot Monday [5] (TimeOfDay 23 0 0) (TimeOfDay 1 0 0) Nothing,
                ParsedScheduleSlot Tuesday [1] (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldSatisfy` isLeft

      it "allows a week-1 tail beside a week-4 slot" $ do
        -- A week-1 Monday is followed by a week-1 or week-2 Tuesday, never week 4.
        let slots =
              [ ParsedScheduleSlot Monday [1] (TimeOfDay 23 0 0) (TimeOfDay 1 0 0) Nothing,
                ParsedScheduleSlot Tuesday [4] (TimeOfDay 0 0 0) (TimeOfDay 1 0 0) Nothing
              ]
        validateNoOverlaps slots `shouldBe` Right slots

    -- The checks above are hand-picked. These two compare the whole of
    -- validateNoOverlaps against 'calendarOverlap', which reaches the answer a
    -- different way: it lays both slots on a real calendar and looks for two
    -- occurrences that intersect.
    describe "validateNoOverlaps against a calendar" $ do
      modifyMaxSuccess (const 500) $
        it "never misses an overlap the calendar produces" $
          hedgehog $ do
            slot1 <- forAll genFormSlot
            slot2 <- forAll genFormSlot
            if calendarOverlap slot1 slot2
              then assert (isLeft (validateNoOverlaps [slot1, slot2]))
              else success

      -- A uniform generator almost never lands two slots on adjacent days at
      -- the hours where a slot can cross midnight. This one aims at that region.
      modifyMaxSuccess (const 2000) $
        it "never misses an overlap around midnight" $
          hedgehog $ do
            (slot1, slot2) <- forAll genMidnightPair
            if calendarOverlap slot1 slot2
              then assert (isLeft (validateNoOverlaps [slot1, slot2]))
              else success

      -- With no slot crossing midnight there is no week widening, so the two
      -- models have to agree in both directions.
      modifyMaxSuccess (const 500) $
        it "agrees exactly when no slot crosses midnight" $
          hedgehog $ do
            slot1 <- forAll (Gen.filter withinOneDay genFormSlot)
            slot2 <- forAll (Gen.filter withinOneDay genFormSlot)
            isLeft (validateNoOverlaps [slot1, slot2]) === calendarOverlap slot1 slot2

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

-- | Create a ScheduleTemplate Result for testing (id 1, no replay).
mkTemplate :: Maybe DayOfWeek -> Maybe [Int64] -> TimeOfDay -> TimeOfDay -> ShowSchedule.ScheduleTemplate Rel8.Result
mkTemplate dow weeks start end =
  mkTemplateWith (ShowSchedule.TemplateId 1) dow weeks start end Nothing

-- | Create a ScheduleTemplate Result with an explicit id and replay time.
mkTemplateWith ::
  ShowSchedule.TemplateId ->
  Maybe DayOfWeek ->
  Maybe [Int64] ->
  TimeOfDay ->
  TimeOfDay ->
  Maybe TimeOfDay ->
  ShowSchedule.ScheduleTemplate Rel8.Result
mkTemplateWith tid dow weeks start end replay =
  ShowSchedule.ScheduleTemplate
    { stId = tid,
      stShowId = Shows.Id 1,
      stDayOfWeek = dow,
      stWeeksOfMonth = weeks,
      stStartTime = start,
      stEndTime = end,
      stTimezone = "America/Los_Angeles",
      stCreatedAt = UTCTime (fromGregorian 2025 1 1) 0,
      stReplayStartTime = replay
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

-- | A slot the schedule editor can actually produce.
--
-- The time picker offers every half hour, and the duration buttons offer 30, 60,
-- and 120 minutes. A late start therefore gives a slot that crosses midnight.
genFormSlot :: Gen ParsedScheduleSlot
genFormSlot = do
  day <- Gen.element [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]
  weeks <- sort <$> Gen.filter (not . null) (Gen.subsequence [1, 2, 3, 4, 5])
  halfHours <- Gen.int (Range.linear 0 47)
  duration <- Gen.element [30, 60, 120]
  let start = TimeOfDay (halfHours `div` 2) (30 * (halfHours `mod` 2)) 0
  pure $
    ParsedScheduleSlot
      { pssDay = day,
        pssWeeks = weeks,
        pssStart = start,
        pssEnd = addMinutesToTimeOfDay start duration,
        pssReplayStartTime = Nothing
      }

-- | A pair of slots aimed at the region where a slot can cross midnight.
--
-- The second slot sits on the day before, the same day, or the day after the
-- first. Both start late in the evening or early in the morning. A uniform
-- generator reaches this region about twice in ten thousand draws.
genMidnightPair :: Gen (ParsedScheduleSlot, ParsedScheduleSlot)
genMidnightPair = do
  day <- Gen.element [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]
  offset <- Gen.element [-1, 0, 1]
  slot1 <- genEdgeSlot day
  slot2 <- genEdgeSlot (toEnum (fromEnum day + offset))
  pure (slot1, slot2)

-- | A slot on a given day that starts between 21:00 and 03:00.
genEdgeSlot :: DayOfWeek -> Gen ParsedScheduleSlot
genEdgeSlot day = do
  weeks <- sort <$> Gen.filter (not . null) (Gen.subsequence [1, 2, 3, 4, 5])
  halfHours <- Gen.element ([42 .. 47] <> [0 .. 6])
  duration <- Gen.element [30, 60, 120]
  let start = TimeOfDay (halfHours `div` 2) (30 * (halfHours `mod` 2)) 0
  pure $
    ParsedScheduleSlot
      { pssDay = day,
        pssWeeks = weeks,
        pssStart = start,
        pssEnd = addMinutesToTimeOfDay start duration,
        pssReplayStartTime = Nothing
      }

--------------------------------------------------------------------------------
-- A calendar model of slot overlap

-- | The first date of the window the calendar model covers.
calendarStart :: Day
calendarStart = fromGregorian 2026 1 1

-- | The length of that window. Three years covers every month length, including
-- a 28-day February, which is the only way a week-4 date can be a month end.
calendarDays :: Int
calendarDays = 1096

-- | The week of the month a date falls in. Days 1 to 7 are week 1.
weekOfMonth :: Day -> Int64
weekOfMonth day =
  let (_, _, dayOfMonth) = toGregorian day
   in fromIntegral ((dayOfMonth - 1) `div` 7 + 1)

-- | Every occurrence of a slot in the calendar window, as a range of minutes
-- from midnight on 'calendarStart'.
occurrences :: ParsedScheduleSlot -> [(Integer, Integer)]
occurrences slot =
  [ (begin, begin + fromIntegral (slotDurationMins (pssStart slot) (pssEnd slot)))
  | day <- take calendarDays (iterate (addDays 1) calendarStart),
    Time.dayOfWeek day == pssDay slot,
    weekOfMonth day `elem` pssWeeks slot,
    let begin =
          diffDays day calendarStart * 1440
            + fromIntegral (todHour (pssStart slot) * 60 + todMin (pssStart slot))
  ]

-- | Whether two slots ever air at the same time on a real calendar.
--
-- This reaches the answer without the day-of-week and week-of-month reasoning
-- that 'validateNoOverlaps' uses. It walks real dates instead.
calendarOverlap :: ParsedScheduleSlot -> ParsedScheduleSlot -> Bool
calendarOverlap slot1 slot2 =
  or
    [ begin1 < end2 && begin2 < end1
    | (begin1, end1) <- occurrences slot1,
      (begin2, end2) <- occurrences slot2
    ]

-- | Whether a slot stays inside the day it starts on.
withinOneDay :: ParsedScheduleSlot -> Bool
withinOneDay slot =
  todHour (pssStart slot) * 60
    + todMin (pssStart slot)
    + slotDurationMins (pssStart slot) (pssEnd slot)
    <= 1440
