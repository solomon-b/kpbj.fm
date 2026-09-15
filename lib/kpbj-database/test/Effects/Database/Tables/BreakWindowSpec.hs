-- | Tests for the isBreakDue query.
--
-- Liquidsoap asks at @:28@ and @:58@ whether a break is due at the boundary two
-- minutes later. A wrong yes cuts a show mid sentence. A wrong no drops the
-- PSAs and paid spots that were meant to air.
--
-- A break is due when the boundary is either the end of a scheduled slot, or
-- the top of an hour that no slot spans. These tests cover:
--
-- 1. Automation hours, where every hour turns but no half hour does
-- 2. The three slot lengths, 30 minutes, 1 hour, and 2 hours
-- 3. The midpoint of a 2 hour slot, which must not break
-- 4. An overnight slot, whose end falls on the next date
-- 5. A replay slot, which breaks at its own end
-- 6. Slots that do not count: closed validity, wrong recurrence, inactive show,
--    and deleted show
module Effects.Database.Tables.BreakWindowSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time
  ( Day,
    DayOfWeek (..),
    LocalTime (..),
    TimeOfDay (..),
    UTCTime (..),
    addDays,
    dayOfWeek,
    fromGregorian,
  )
import Domain.Types.Slug (mkSlug)
import Domain.Types.Timezone (pacificToUtc)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Hspec (Spec, describe, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.ShowSchedule.isBreakDue" $ do
      describe "automation hours, with nothing scheduled" $ do
        it "breaks at the top of the hour" emptyTopOfHour
        it "does not break at the half hour" emptyHalfHour

      describe "a slot ending at the boundary" $ do
        it "breaks at the end of a 30 minute slot" slotEnd30
        it "breaks at the end of a 1 hour slot" slotEnd60
        it "breaks at the end of a 2 hour slot" slotEnd120
        it "breaks at the end of an overnight slot, on the next date" slotEndOvernight
        it "breaks at the end of a replay slot" slotEndReplay

      describe "a slot spanning the boundary" $ do
        it "does not break at the midpoint of a 2 hour slot" spanningMidpoint
        it "does not break at the half hour inside a slot" spanningHalfHour
        it "does not break at the start of a slot" spanningSlotStart

      describe "slots that do not count" $ do
        it "breaks when the spanning slot's validity has closed" ignoredClosedValidity
        it "breaks when the spanning slot does not recur on this date" ignoredWrongRecurrence
        it "breaks when the spanning show is inactive" ignoredInactiveShow
        it "breaks when the spanning show is soft-deleted" ignoredDeletedShow

--------------------------------------------------------------------------------
-- Test Helpers

-- | A test date: Monday, January 6, 2025.
--
-- Well clear of both daylight saving transitions, so these cases read as plain
-- window arithmetic.
testDay :: Day
testDay = fromGregorian 2025 1 6

-- | A Pacific wall clock time on the test day, as an instant.
boundaryAt :: TimeOfDay -> UTCTime
boundaryAt tod = pacificToUtc (LocalTime testDay tod)

-- | A Pacific wall clock time on the day after the test day.
boundaryNextDayAt :: TimeOfDay -> UTCTime
boundaryNextDayAt tod = pacificToUtc (LocalTime (addDays 1 testDay) tod)

-- | Insert a show and give it one slot.
--
-- The recurrence is every week on the weekday of 'testDay', so the slot always
-- covers the dates these cases ask about, and each case exercises the window
-- arithmetic rather than the recurrence.
addSlot ::
  -- | Slug suffix, so each show in a test is distinct
  Text ->
  TimeOfDay ->
  TimeOfDay ->
  -- | Replay start time
  Maybe TimeOfDay ->
  TRX.Transaction ()
addSlot = addSlotWith Shows.Active Nothing Nothing

-- | 'addSlot', with control over what makes a slot count.
addSlotWith ::
  Shows.Status ->
  -- | Recurrence weekday. Nothing takes the weekday of 'testDay'
  Maybe DayOfWeek ->
  -- | Validity effective_until. Nothing leaves the window open
  Maybe Day ->
  Text ->
  TimeOfDay ->
  TimeOfDay ->
  Maybe TimeOfDay ->
  TRX.Transaction ()
addSlotWith status mWeekday mEffectiveUntil slugSuffix startTime endTime replayStartTime = do
  showId <-
    unwrapInsert $
      Shows.insertShow
        Shows.Insert
          { siTitle = "Test Show " <> slugSuffix,
            siSlug = mkSlug ("test-show-" <> slugSuffix),
            siDescription = Nothing,
            siLogoUrl = Nothing,
            siStatus = status
          }

  templateId <-
    TRX.statement () $
      ShowSchedule.insertScheduleTemplate
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = showId,
            stiDayOfWeek = fromMaybe (dayOfWeek testDay) mWeekday,
            stiWeeksOfMonth = [1, 2, 3, 4, 5],
            stiStartTime = startTime,
            stiEndTime = endTime,
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = replayStartTime
          }

  _ <-
    unwrapInsert $
      ShowSchedule.insertValidity
        ShowSchedule.ValidityInsert
          { viTemplateId = templateId,
            viEffectiveFrom = addDays (-7) testDay,
            viEffectiveUntil = mEffectiveUntil
          }
  pure ()

-- | Soft delete every show, so no slot counts.
deleteAllShows :: TRX.Transaction ()
deleteAllShows = TRX.sql "UPDATE shows SET deleted_at = NOW()"

-- | Build the fixture, ask about one boundary, and compare.
checkBreak ::
  TestDBConfig ->
  -- | Fixture
  TRX.Transaction () ->
  -- | The boundary to ask about
  UTCTime ->
  -- | Whether a break should be due
  Bool ->
  IO ()
checkBreak cfg fixture breakEnd expected = bracketConn cfg $ do
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    fixture
    TRX.statement () $ ShowSchedule.isBreakDue breakEnd

  case result of
    Left err -> error $ "DB error: " <> show err
    Right due -> liftIO $ due `shouldBe` expected

--------------------------------------------------------------------------------
-- Automation Hours

-- | Nothing is scheduled, so the hour turns with no slot spanning it.
emptyTopOfHour :: TestDBConfig -> IO ()
emptyTopOfHour cfg =
  checkBreak cfg (pure ()) (boundaryAt (TimeOfDay 11 0 0)) True

-- | A half hour is not the top of an hour, and no slot ends there.
emptyHalfHour :: TestDBConfig -> IO ()
emptyHalfHour cfg =
  checkBreak cfg (pure ()) (boundaryAt (TimeOfDay 10 30 0)) False

--------------------------------------------------------------------------------
-- Slot Ends

slotEnd30 :: TestDBConfig -> IO ()
slotEnd30 cfg =
  checkBreak
    cfg
    (addSlot "a" (TimeOfDay 10 0 0) (TimeOfDay 10 30 0) Nothing)
    (boundaryAt (TimeOfDay 10 30 0))
    True

slotEnd60 :: TestDBConfig -> IO ()
slotEnd60 cfg =
  checkBreak
    cfg
    (addSlot "a" (TimeOfDay 10 0 0) (TimeOfDay 11 0 0) Nothing)
    (boundaryAt (TimeOfDay 11 0 0))
    True

slotEnd120 :: TestDBConfig -> IO ()
slotEnd120 cfg =
  checkBreak
    cfg
    (addSlot "a" (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing)
    (boundaryAt (TimeOfDay 12 0 0))
    True

-- | A slot from 23:00 to 01:00 closes on the date after its air date.
slotEndOvernight :: TestDBConfig -> IO ()
slotEndOvernight cfg =
  checkBreak
    cfg
    (addSlot "a" (TimeOfDay 23 0 0) (TimeOfDay 1 0 0) Nothing)
    (boundaryNextDayAt (TimeOfDay 1 0 0))
    True

-- | A replay runs for the same length as its primary, so a 1 hour slot
-- replaying at 20:00 closes at 21:00.
slotEndReplay :: TestDBConfig -> IO ()
slotEndReplay cfg =
  checkBreak
    cfg
    (addSlot "a" (TimeOfDay 10 0 0) (TimeOfDay 11 0 0) (Just (TimeOfDay 20 0 0)))
    (boundaryAt (TimeOfDay 21 0 0))
    True

--------------------------------------------------------------------------------
-- Slots Spanning The Boundary

-- | The case the whole rule exists for.
--
-- A 2 hour show delivers about 118 minutes of audio. Breaking at its midpoint
-- would cut the file in half, so the top of that hour must pass untouched.
spanningMidpoint :: TestDBConfig -> IO ()
spanningMidpoint cfg =
  checkBreak
    cfg
    (addSlot "a" (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing)
    (boundaryAt (TimeOfDay 11 0 0))
    False

spanningHalfHour :: TestDBConfig -> IO ()
spanningHalfHour cfg =
  checkBreak
    cfg
    (addSlot "a" (TimeOfDay 10 0 0) (TimeOfDay 11 0 0) Nothing)
    (boundaryAt (TimeOfDay 10 30 0))
    False

-- | A slot opening at the boundary is not a slot ending at it.
--
-- The break belongs to the show that ends, so automation runs into a show with
-- no break in front of it.
spanningSlotStart :: TestDBConfig -> IO ()
spanningSlotStart cfg =
  checkBreak
    cfg
    (addSlot "a" (TimeOfDay 11 0 0) (TimeOfDay 12 0 0) Nothing)
    (boundaryAt (TimeOfDay 11 0 0))
    True

--------------------------------------------------------------------------------
-- Slots That Do Not Count
--
-- Each of these puts a 2 hour slot across 11:00, then spoils it. The break at
-- 11:00 comes back, which shows the slot stopped counting.

ignoredClosedValidity :: TestDBConfig -> IO ()
ignoredClosedValidity cfg =
  checkBreak
    cfg
    ( addSlotWith
        Shows.Active
        Nothing
        (Just (addDays (-1) testDay))
        "a"
        (TimeOfDay 10 0 0)
        (TimeOfDay 12 0 0)
        Nothing
    )
    (boundaryAt (TimeOfDay 11 0 0))
    True

ignoredWrongRecurrence :: TestDBConfig -> IO ()
ignoredWrongRecurrence cfg =
  checkBreak
    cfg
    ( addSlotWith
        Shows.Active
        (Just (nextWeekday (dayOfWeek testDay)))
        Nothing
        "a"
        (TimeOfDay 10 0 0)
        (TimeOfDay 12 0 0)
        Nothing
    )
    (boundaryAt (TimeOfDay 11 0 0))
    True

ignoredInactiveShow :: TestDBConfig -> IO ()
ignoredInactiveShow cfg =
  checkBreak
    cfg
    ( addSlotWith
        Shows.Inactive
        Nothing
        Nothing
        "a"
        (TimeOfDay 10 0 0)
        (TimeOfDay 12 0 0)
        Nothing
    )
    (boundaryAt (TimeOfDay 11 0 0))
    True

ignoredDeletedShow :: TestDBConfig -> IO ()
ignoredDeletedShow cfg =
  checkBreak
    cfg
    ( do
        addSlot "a" (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) Nothing
        deleteAllShows
    )
    (boundaryAt (TimeOfDay 11 0 0))
    True

-- | The weekday after this one, for a recurrence the test date cannot match.
nextWeekday :: DayOfWeek -> DayOfWeek
nextWeekday = \case
  Monday -> Tuesday
  Tuesday -> Wednesday
  Wednesday -> Thursday
  Thursday -> Friday
  Friday -> Saturday
  Saturday -> Sunday
  Sunday -> Monday
