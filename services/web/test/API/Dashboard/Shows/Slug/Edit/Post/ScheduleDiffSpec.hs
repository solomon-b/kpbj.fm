-- | Pure unit tests for schedule diff logic.
--
-- Tests that schedulesMatch correctly detects when form schedules match
-- the existing DB templates, preventing unnecessary terminate-and-recreate cycles.
-- Also includes property tests for the set-difference algebra used by
-- slot-level diffing in updateScheduleTemplates.
module API.Dashboard.Shows.Slug.Edit.Post.ScheduleDiffSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Post.Handler (ParsedScheduleSlot (..), normalizeTemplate, parseScheduleSlot, removedTemplates, scheduleUpdateFlash, schedulesMatch, validateSingleSlot)
import API.Dashboard.Shows.Slug.Edit.Post.Route (ScheduleSlotInfo (..))
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..))
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (DayOfWeek (..), TimeOfDay (..), UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Domain.Types.Recurrence (editorWeekSets, recurrenceFromRow, weekNumbers)
import Effects.Database.Tables.Episodes qualified as Episodes
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
        let template = mkTemplate Friday [3, 1, 2] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)
        normalizeTemplate template
          `shouldBe` mkParsed Friday [1, 2, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) Nothing

      -- Must match what parseScheduleSlot does to the form's weeks. A stored
      -- duplicate that normalized to [1,1,3] would stop matching its own form value
      -- of [1,3], and the diff would close the slot and build it again, detaching
      -- every upcoming episode on it.
      it "deduplicates a stored duplicate the same way the form side does" $ do
        let template = mkTemplate Friday [3, 1, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)
            formSlot = mkSlot "friday" [3, 1, 3] "08:00" 120
        normalizeTemplate template
          `shouldBe` mkParsed Friday [1, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) Nothing
        Right True `shouldBe` fmap (schedulesMatch [template] . Just) (parseScheduleSlot formSlot)

    describe "parseScheduleSlot" $ do
      it "parses and normalizes a valid form slot" $ do
        let slot = mkSlot "friday" [3, 1] "08:00" 120
        parseScheduleSlot slot
          `shouldBe` Right (mkParsed Friday [1, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) Nothing)

      it "rejects invalid start time" $ do
        let slot = mkSlot "friday" [1, 3] "invalid" 60
        parseScheduleSlot slot `shouldBe` Left "Invalid start time: invalid"

      it "rejects invalid duration" $ do
        let slot = mkSlot "friday" [1, 3] "08:00" 45
        parseScheduleSlot slot `shouldBe` Left "Invalid duration: 45 (must be 30, 60, or 120)"

      it "rejects unknown day of week" $ do
        let slot = mkSlot "funday" [1] "08:00" 60
        parseScheduleSlot slot `shouldBe` Left "Invalid day of week: funday"

      -- The weeks came straight from the form JSON with no check of their own. The
      -- database CHECK caught an out-of-range week, but only at insert time, after
      -- the diff had already closed the templates it was replacing. It never caught
      -- an empty list at all, because array_length of an empty array is NULL and a
      -- CHECK passes on NULL.
      it "rejects an empty weeks list" $ do
        -- An empty list is a slot that never airs and never conflicts. Every date
        -- fails the ANY(weeks) test.
        let slot = mkSlot "friday" [] "08:00" 60
        parseScheduleSlot slot `shouldBe` Left "Pick at least one week of the month."

      it "rejects a week above 5" $ do
        let slot = mkSlot "friday" [6] "08:00" 60
        parseScheduleSlot slot `shouldBe` Left "Invalid week of month: 6 (must be 1 to 5)."

      it "rejects a week below 1" $ do
        let slot = mkSlot "friday" [0] "08:00" 60
        parseScheduleSlot slot `shouldBe` Left "Invalid week of month: 0 (must be 1 to 5)."

      it "names every out-of-range week" $ do
        let slot = mkSlot "friday" [1, 7, 0] "08:00" 60
        parseScheduleSlot slot `shouldBe` Left "Invalid week of month: 7, 0 (must be 1 to 5)."

      it "accepts every week from 1 to 5" $ do
        let slot = mkSlot "friday" [1, 2, 3, 4, 5] "08:00" 60
        fmap (weekNumbers . pssRecurrence) (parseScheduleSlot slot) `shouldBe` Right [1, 2, 3, 4, 5]

      it "deduplicates repeated weeks" $ do
        let slot = mkSlot "friday" [3, 1, 3] "08:00" 60
        fmap (weekNumbers . pssRecurrence) (parseScheduleSlot slot) `shouldBe` Right [1, 3]

    describe "week sets the schedule editor cannot show" $ do
      -- weeks_of_month keeps all 31 non-empty subsets of 1 to 5 so the editor can
      -- grow into them. Until it does, the parse boundary only lets through the seven
      -- it can render and post back, because a stored value outside those seven shows
      -- a frequency button with no week button beside it.
      it "rejects a three-week set" $ do
        parseScheduleSlot (mkSlot "friday" [1, 2, 3] "08:00" 60)
          `shouldBe` Left "The schedule form cannot show 1st & 2nd & 3rd. Pick every week, the 1st and 3rd, the 2nd and 4th, or one week from the 1st to the 4th."

      it "rejects a two-week set the buttons do not offer" $ do
        parseScheduleSlot (mkSlot "friday" [1, 4] "08:00" 60) `shouldSatisfy` isLeft

      it "rejects the 5th week, which has no button" $ do
        parseScheduleSlot (mkSlot "friday" [5] "08:00" 60) `shouldSatisfy` isLeft

      it "accepts every set the editor emits" $
        mapM_
          (\weeks -> parseScheduleSlot (mkSlot "friday" weeks "08:00" 60) `shouldSatisfy` isRight)
          editorWeekSets

      -- Reading is not writing. A row already holding one of the other 24 still
      -- normalizes, so the show renders and a schedule change can still close it.
      it "still normalizes a stored set the editor cannot show" $ do
        let template = mkTemplate Friday [1, 2, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)
        normalizeTemplate template
          `shouldBe` mkParsed Friday [1, 2, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) Nothing

    describe "schedulesMatch" $ do
      it "returns True for identical schedules" $ do
        let templates = [mkTemplate Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slot = mkParsed Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
        schedulesMatch templates (Just slot) `shouldBe` True

      it "returns True when the DB template has unsorted weeks" $ do
        let templates = [mkTemplate Friday [5, 1, 3] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slot = mkParsed Friday [1, 3, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
        schedulesMatch templates (Just slot) `shouldBe` True

      it "returns False for different day of week" $ do
        let templates = [mkTemplate Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slot = mkParsed Saturday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
        schedulesMatch templates (Just slot) `shouldBe` False

      it "returns False for different weeks of month" $ do
        let templates = [mkTemplate Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slot = mkParsed Friday [1, 3] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
        schedulesMatch templates (Just slot) `shouldBe` False

      it "returns False for different start time" $ do
        let templates = [mkTemplate Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slot = mkParsed Friday [1, 2, 3, 4, 5] (TimeOfDay 20 0 0) (TimeOfDay 21 0 0) Nothing
        schedulesMatch templates (Just slot) `shouldBe` False

      it "returns False for different end time" $ do
        let templates = [mkTemplate Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
            slot = mkParsed Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 22 0 0) Nothing
        schedulesMatch templates (Just slot) `shouldBe` False

      it "returns False for different replay time" $ do
        let templates = [mkTemplateWith (ShowSchedule.TemplateId 1) Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) (Just (TimeOfDay 2 0 0))]
            slot = mkParsed Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) (Just (TimeOfDay 3 0 0))
        schedulesMatch templates (Just slot) `shouldBe` False

      -- A database written before one_active_slot_per_show can still hold two active
      -- templates for one show. The form carries one, so the two cannot match.
      it "returns False when the DB holds more slots than the form" $ do
        let templates =
              [ mkTemplateWith (ShowSchedule.TemplateId 1) Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing,
                mkTemplateWith (ShowSchedule.TemplateId 2) Monday [1, 2, 3, 4, 5] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) Nothing
              ]
            slot = mkParsed Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
        schedulesMatch templates (Just slot) `shouldBe` False

      it "returns True when neither side holds a slot" $
        schedulesMatch [] Nothing `shouldBe` True

      it "returns False when the DB holds a slot and the form does not" $ do
        let templates = [mkTemplate Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
        schedulesMatch templates Nothing `shouldBe` False

      it "returns False when the form holds a slot and the DB does not" $ do
        let slot = mkParsed Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
        schedulesMatch [] (Just slot) `shouldBe` False

    describe "removedTemplates" $ do
      it "returns the template when the form drops its slot" $ do
        let friday = mkTemplateWith (ShowSchedule.TemplateId 1) Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
        map ShowSchedule.stId (removedTemplates [friday] Nothing) `shouldBe` [ShowSchedule.TemplateId 1]

      it "returns the original template when the slot is re-keyed by changing weeks" $ do
        let template = mkTemplateWith (ShowSchedule.TemplateId 5) Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
            -- Same day and time, but the weeks changed, so the signature no longer matches.
            form = mkParsed Friday [1, 3] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
        map ShowSchedule.stId (removedTemplates [template] (Just form)) `shouldBe` [ShowSchedule.TemplateId 5]

      it "returns the original template when the slot is re-keyed by changing replay" $ do
        let template = mkTemplateWith (ShowSchedule.TemplateId 7) Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) (Just (TimeOfDay 2 0 0))
            form = mkParsed Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) (Just (TimeOfDay 3 0 0))
        map ShowSchedule.stId (removedTemplates [template] (Just form)) `shouldBe` [ShowSchedule.TemplateId 7]

      it "does not return a template whose slot is unchanged" $ do
        let template = mkTemplateWith (ShowSchedule.TemplateId 1) Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
            form = mkParsed Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
        map ShowSchedule.stId (removedTemplates [template] (Just form)) `shouldBe` []

      it "returns nothing when the DB holds no template" $ do
        let form = mkParsed Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
        removedTemplates [] (Just form) `shouldBe` []

      -- normalizeTemplate is total, so no active template escapes the comparison and
      -- a schedule change can close every one it does not keep.
      it "returns every template the form does not keep" $ do
        let a = mkTemplateWith (ShowSchedule.TemplateId 1) Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
            b = mkTemplateWith (ShowSchedule.TemplateId 2) Monday [1, 2, 3, 4, 5] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) Nothing
            form = mkParsed Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0) Nothing
        map ShowSchedule.stId (removedTemplates [a, b] (Just form)) `shouldBe` [ShowSchedule.TemplateId 2]

    describe "validateSingleSlot" $ do
      it "accepts no slots at all" $
        validateSingleSlot [] `shouldBe` Right Nothing

      it "accepts one slot with no replay" $ do
        let one = mkParsed Monday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing
        validateSingleSlot [one] `shouldBe` Right (Just one)

      it "accepts a replay that starts when the airing ends" $ do
        let one = mkParsed Monday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) (Just (TimeOfDay 12 0 0))
        validateSingleSlot [one] `shouldBe` Right (Just one)

      it "accepts an overnight airing with a replay the next morning" $ do
        let one = mkParsed Monday [1, 2, 3, 4, 5] (TimeOfDay 23 0 0) (TimeOfDay 1 0 0) (Just (TimeOfDay 6 0 0))
        validateSingleSlot [one] `shouldBe` Right (Just one)

      it "rejects a replay that runs over its own airing" $ do
        -- 10:00-12:00 with a replay at 11:00 puts the replay inside the airing.
        let one = mkParsed Monday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) (Just (TimeOfDay 11 0 0))
        validateSingleSlot [one] `shouldSatisfy` isLeft

      it "rejects a replay that starts before its own airing" $ do
        let one = mkParsed Monday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) (Just (TimeOfDay 9 0 0))
        validateSingleSlot [one] `shouldSatisfy` isLeft

      -- one_active_slot_per_show makes two concurrent slots unrepresentable. A form
      -- carrying two is rejected here with a message instead of by the database with
      -- an exclusion violation.
      it "rejects more than one slot" $ do
        let a = mkParsed Monday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing
            b = mkParsed Tuesday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing
        validateSingleSlot [a, b] `shouldSatisfy` isLeft

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
mkTemplate :: DayOfWeek -> [Int64] -> TimeOfDay -> TimeOfDay -> ShowSchedule.ScheduleTemplate Rel8.Result
mkTemplate dow weeks start end =
  mkTemplateWith (ShowSchedule.TemplateId 1) dow weeks start end Nothing

-- | Create a ScheduleTemplate Result with an explicit id and replay time.
mkTemplateWith ::
  ShowSchedule.TemplateId ->
  DayOfWeek ->
  [Int64] ->
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

-- | A parsed slot, with its weeks normalized the way the handler normalizes them.
mkParsed :: DayOfWeek -> [Int64] -> TimeOfDay -> TimeOfDay -> Maybe TimeOfDay -> ParsedScheduleSlot
mkParsed dow weeks start end replay =
  ParsedScheduleSlot
    { pssRecurrence = recurrenceFromRow dow weeks,
      pssStart = start,
      pssEnd = end,
      pssReplayStartTime = replay
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
  weeks <- Gen.filter (not . null) (Gen.subsequence [1, 2, 3, 4, 5])
  startHour <- Gen.int (Range.linear 6 22)
  let endHour = min 23 (startHour + 2)
  mReplay <- Gen.maybe $ do
    replayHour <- Gen.int (Range.linear endHour 23)
    pure $ TimeOfDay replayHour 0 0
  pure $ mkParsed day weeks (TimeOfDay startHour 0 0) (TimeOfDay endHour 0 0) mReplay
