module Test.Gen.Tables.ShowSchedule where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, DayOfWeek (..), LocalTime (..), TimeOfDay (..), UTCTime, addDays)
import Data.Time qualified as Time
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Domain.Types.Timezone (pacificToUtc)
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

-- | Generate weeks of month (1-5).
--
-- Never empty. A template with an empty weeks_of_month matches no date, so the
-- show never airs and its slot is invisible to the conflict check. The table
-- CHECK meant to forbid that uses array_length, which returns NULL for an empty
-- array, so it lets one through.
genWeeksOfMonth :: (MonadGen m) => m [Int64]
genWeeksOfMonth = do
  weeks <- Gen.subsequence @_ @Int64 [1, 2, 3, 4, 5]
  if null weeks
    then (: []) <$> Gen.element [1, 2, 3, 4, 5]
    else pure weeks

-- | The week of the month a date falls in. Days 1 to 7 are week 1.
--
-- The same formula getCurrentlyAiringEpisode uses.
weekOfMonth :: Day -> Int64
weekOfMonth day =
  let (_, _, dayOfMonth) = toGregorian day
   in fromIntegral ((dayOfMonth - 1) `div` 7 + 1)

-- | The first instant on or after @from@ that @template@ airs.
--
-- Takes the template's start time and the first Pacific date whose weekday and
-- week of the month the template covers. An episode fixture built with this
-- satisfies the air-date trigger, which rejects an episode whose date its
-- template does not air on.
--
-- A one-time template airs only on its validity date. Pass that date and get it
-- back with the start time applied.
--
-- The timezone is America/Los_Angeles whatever stiTimezone says, because the
-- trigger and getCurrentlyAiringEpisode both hardcode it.
airTimeForTemplate :: ShowSchedule.ScheduleTemplateInsert -> Day -> UTCTime
airTimeForTemplate template from =
  airTimeOn template (airDayForTemplate template from)

-- | The first date on or after @from@ that @template@ airs.
--
-- A one-time template airs on one date only, so it returns @from@ unchanged and
-- the caller must pass its validity date.
airDayForTemplate :: ShowSchedule.ScheduleTemplateInsert -> Day -> Day
airDayForTemplate template from =
  case ShowSchedule.stiDayOfWeek template of
    Nothing -> from
    Just dow ->
      let weeks = case ShowSchedule.stiWeeksOfMonth template of
            Just ws | not (null ws) -> ws
            _ -> allWeeksOfMonth
          matches =
            [ day
              | day <- take 400 (iterate (addDays 1) from),
                Time.dayOfWeek day == dow,
                weekOfMonth day `elem` weeks
            ]
       in case matches of
            (day : _) -> day
            [] -> from

-- | A template's start time on a given date, as an instant.
--
-- Pair it with 'airDayForTemplate' when a test needs the dates themselves, for
-- example to place a change date between two airings.
airTimeOn :: ShowSchedule.ScheduleTemplateInsert -> Day -> UTCTime
airTimeOn template day =
  pacificToUtc (LocalTime day (ShowSchedule.stiStartTime template))

-- | The last instant strictly before @before@ that @template@ airs.
--
-- The backward counterpart of 'airTimeForTemplate', for a fixture that has to sit
-- in the past. A one-time template airs on one date only, so it returns that date
-- unchanged and the caller must pass one that is already in the past.
lastAirTimeBefore :: ShowSchedule.ScheduleTemplateInsert -> Day -> UTCTime
lastAirTimeBefore template before =
  airTimeOn template airDay
  where
    airDay = case ShowSchedule.stiDayOfWeek template of
      Nothing -> before
      Just dow ->
        let weeks = case ShowSchedule.stiWeeksOfMonth template of
              Just ws | not (null ws) -> ws
              _ -> allWeeksOfMonth
            matches =
              [ day
                | day <- take 400 (iterate (addDays (-1)) (addDays (-1) before)),
                  Time.dayOfWeek day == dow,
                  weekOfMonth day `elem` weeks
              ]
         in case matches of
              (day : _) -> day
              [] -> addDays (-1) before

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
