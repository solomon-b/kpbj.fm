module Test.Gen.Tables.ShowSchedule where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, DayOfWeek (..), TimeOfDay (..), addDays)
import Data.Time.Clock (getCurrentTime, utctDay)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

-- | All weeks of month (1-5)
--
-- For recurring shows (where dayOfWeek is Just), this represents a weekly schedule.
--
-- Functionally equivalent to weeksOfMonth = NULL for weekly shows:
-- - NULL: "every occurrence of this weekday" (simpler, recommended)
-- - [1,2,3,4,5]: "weeks 1-5 of each month" (explicit, same result)
--
-- For N-of-month shows (e.g., "first and third Monday"), use subsets like [1,3].
--
-- Note: For one-time shows, BOTH dayOfWeek and weeksOfMonth must be NULL.
allWeeksOfMonth :: [Int64]
allWeeksOfMonth = [1, 2, 3, 4, 5]

-- | Generate a DayOfWeek
genDayOfWeek :: (MonadGen m) => m DayOfWeek
genDayOfWeek = Gen.element [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

-- | Generate a valid time range (start < end)
genTimeRange :: (MonadGen m) => m (TimeOfDay, TimeOfDay)
genTimeRange = do
  startHour <- Gen.integral (Range.linear 6 22)
  endHour <- Gen.integral (Range.linear (startHour + 1) 23)
  startMinute <- Gen.element [0, 15, 30, 45]
  endMinute <- Gen.element [0, 15, 30, 45]
  pure (TimeOfDay startHour startMinute 0, TimeOfDay endHour endMinute 0)

-- | Generate weeks of month (1-5)
genWeeksOfMonth :: (MonadGen m) => m [Int64]
genWeeksOfMonth = do
  weeks <- Gen.subsequence @_ @Int64 [1, 2, 3, 4, 5]
  pure $ map fromIntegral weeks

-- | Generate a future Day value
genFutureDay :: (MonadIO m, MonadGen m) => m Day
genFutureDay = do
  today <- liftIO $ utctDay <$> getCurrentTime
  daysAhead <- Gen.integral (Range.linear 1 365)
  pure $ addDays daysAhead today

-- | Generate a timezone string
genTimezone :: (MonadGen m) => m Text
genTimezone =
  Gen.element
    [ "America/Los_Angeles",
      "America/New_York",
      "America/Chicago",
      "America/Denver",
      "UTC"
    ]

-- | Generate a valid replay start time that is >= end time.
--
-- Converts end time to total minutes to ensure replay is never before end,
-- even when end has non-zero minutes (e.g., end = 22:30 won't generate 22:00).
genReplayStartTime :: (MonadGen m) => TimeOfDay -> m (Maybe TimeOfDay)
genReplayStartTime endTime = do
  hasReplay <- Gen.bool
  if hasReplay
    then do
      let endMins = todHour endTime * 60 + todMin endTime
          -- Round up to next 30-minute boundary if not already aligned
          minReplayMins = if endMins `mod` 30 == 0 then endMins else endMins + (30 - endMins `mod` 30)
          maxMins = 23 * 60 + 30 -- 23:30
      if minReplayMins > maxMins
        then pure Nothing -- Not enough room for a replay
        else do
          -- Generate in 30-minute increments from minReplayMins to maxMins
          let slots = [minReplayMins, minReplayMins + 30 .. maxMins]
          replayMins <- Gen.element slots
          pure $ Just (TimeOfDay (replayMins `div` 60) (replayMins `mod` 60) 0)
    else pure Nothing

-- | Generate a recurring schedule template insert.
-- For recurring shows, both day_of_week and weeks_of_month must be NOT NULL.
genRecurringScheduleInsert :: (MonadGen m) => Shows.Id -> m ShowSchedule.ScheduleTemplateInsert
genRecurringScheduleInsert showId = do
  (startTime, endTime) <- genTimeRange
  dayOfWeek <- Just <$> genDayOfWeek
  weeksOfMonth <- Just <$> genWeeksOfMonth -- Must be Just for recurring shows
  timezone <- genTimezone
  replayStartTime <- genReplayStartTime endTime
  pure
    ShowSchedule.ScheduleTemplateInsert
      { stiShowId = showId,
        stiDayOfWeek = dayOfWeek,
        stiWeeksOfMonth = weeksOfMonth,
        stiStartTime = startTime,
        stiEndTime = endTime,
        stiTimezone = timezone,
        stiReplayStartTime = replayStartTime
      }

-- | Generate a one-time schedule template insert
genOneTimeScheduleInsert :: (MonadGen m) => Shows.Id -> m ShowSchedule.ScheduleTemplateInsert
genOneTimeScheduleInsert showId = do
  (startTime, endTime) <- genTimeRange
  timezone <- genTimezone
  replayStartTime <- genReplayStartTime endTime
  pure
    ShowSchedule.ScheduleTemplateInsert
      { stiShowId = showId,
        stiDayOfWeek = Nothing,
        stiWeeksOfMonth = Nothing,
        stiStartTime = startTime,
        stiEndTime = endTime,
        stiTimezone = timezone,
        stiReplayStartTime = replayStartTime
      }

-- | Generate any schedule template insert (recurring or one-time)
scheduleTemplateInsertGen :: (MonadGen m) => Shows.Id -> m ShowSchedule.ScheduleTemplateInsert
scheduleTemplateInsertGen showId =
  Gen.choice [genRecurringScheduleInsert showId, genOneTimeScheduleInsert showId]
