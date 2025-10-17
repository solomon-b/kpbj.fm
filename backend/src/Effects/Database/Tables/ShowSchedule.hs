{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.ShowSchedule where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, displayBuilder)
import Data.Time (Day, DayOfWeek, UTCTime)
import {-# SOURCE #-} Effects.Database.Tables.Shows qualified as Shows
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Database Model

newtype Id = Id Int64
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      ToJSON,
      FromJSON,
      Display,
      DecodeValue,
      EncodeValue
    )

data Model = Model
  { id :: Id,
    showId :: Id,
    dayOfWeek :: Int64,
    startTime :: Text,
    endTime :: Text,
    timezone :: Text,
    isActive :: Bool,
    effectiveFrom :: Day,
    effectiveUntil :: Maybe Day,
    createdAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display Model where
  displayBuilder _ = "ShowScheduleModel"

data Insert = Insert
  { ssiId :: Id,
    ssiDayOfWeek :: Int64,
    ssiStartTime :: Text,
    ssiEndTime :: Text,
    ssiTimezone :: Text,
    ssiIsActive :: Bool,
    ssiEffectiveFrom :: Day,
    ssiEffectiveUntil :: Maybe Day
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)

-- | Data type to represent an upcoming show date (with proper DayOfWeek type)
data UpcomingShowDate = UpcomingShowDate
  { usdId :: Shows.Id,
    usdShowDate :: Day,
    usdDayOfWeek :: DayOfWeek,
    usdStartTime :: UTCTime,
    usdEndTime :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

instance Display UpcomingShowDate where
  displayBuilder _ = "UpcomingShowDate"

-- | Convert from database row to public type
fromUpcomingShowDateRow :: (Shows.Id, Day, Int64, UTCTime, UTCTime) -> UpcomingShowDate
fromUpcomingShowDateRow (showId, showDate, dayOfWeek, startTime, endTime) =
  UpcomingShowDate
    { usdId = showId,
      usdShowDate = showDate,
      usdDayOfWeek = toEnum (fromIntegral dayOfWeek),
      usdStartTime = startTime,
      usdEndTime = endTime
    }

--------------------------------------------------------------------------------
-- Database Queries

-- | Get schedules for a show
getSchedulesForShow :: Shows.Id -> Hasql.Statement () [Model]
getSchedulesForShow showId =
  interp
    False
    [sql|
    SELECT id, show_id, day_of_week, start_time, end_time, timezone, is_active, effective_from, effective_until, created_at
    FROM show_schedules
    WHERE show_id = #{showId} AND is_active = true
      AND (effective_until IS NULL OR effective_until >= CURRENT_DATE)
    ORDER BY day_of_week, start_time
  |]

-- | Get current weekly schedule (all active shows) - just schedules
getCurrentWeeklySchedule :: Hasql.Statement () [Model]
getCurrentWeeklySchedule =
  interp
    False
    [sql|
    SELECT ss.id, ss.show_id, ss.day_of_week, ss.start_time, ss.end_time, ss.timezone, ss.is_active, ss.effective_from, ss.effective_until, ss.created_at
    FROM show_schedules ss
    JOIN shows s ON s.id = ss.show_id
    WHERE s.status = 'active' AND ss.is_active = true
      AND (ss.effective_until IS NULL OR ss.effective_until >= CURRENT_DATE)
    ORDER BY ss.day_of_week, ss.start_time
  |]

-- | Insert a new show schedule
insertShowSchedule :: Insert -> Hasql.Statement () Id
insertShowSchedule Insert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO show_schedules(show_id, day_of_week, start_time, end_time, timezone, is_active, effective_from, effective_until, created_at)
    VALUES (#{ssiId}, #{ssiDayOfWeek}, #{ssiStartTime}, #{ssiEndTime}, #{ssiTimezone}, #{ssiIsActive}, #{ssiEffectiveFrom}, #{ssiEffectiveUntil}, NOW())
    RETURNING id
  |]

-- | Update show schedule
updateShowSchedule :: Id -> Insert -> Hasql.Statement () (Maybe Id)
updateShowSchedule scheduleId Insert {..} =
  interp
    False
    [sql|
    UPDATE show_schedules
    SET show_id = #{ssiId}, day_of_week = #{ssiDayOfWeek}, start_time = #{ssiStartTime},
        end_time = #{ssiEndTime}, timezone = #{ssiTimezone}, is_active = #{ssiIsActive},
        effective_from = #{ssiEffectiveFrom}, effective_until = #{ssiEffectiveUntil}
    WHERE id = #{scheduleId}
    RETURNING id
  |]

-- | Deactivate show schedule
deleteShowSchedule :: Id -> Hasql.Statement () (Maybe Id)
deleteShowSchedule scheduleId =
  interp
    False
    [sql|
    UPDATE show_schedules
    SET is_active = false
    WHERE id = #{scheduleId}
    RETURNING id
  |]

-- | Get the next N upcoming scheduled dates for a specific show
-- Starting from today's date, calculates the next occurrences of the show based on its schedule
getUpcomingShowDates :: Shows.Id -> Int64 -> Hasql.Statement () [UpcomingShowDate]
getUpcomingShowDates showId limit =
  fmap fromUpcomingShowDateRow
    <$> interp
      False
      [sql|
    WITH RECURSIVE upcoming_dates AS (
      -- Base case: Get all active schedules for this show
      SELECT
        ss.show_id,
        ss.day_of_week,
        ss.start_time,
        ss.end_time,
        ss.timezone,
        -- Calculate next occurrence of this day of week from today
        CASE
          WHEN ss.day_of_week = EXTRACT(DOW FROM CURRENT_DATE) THEN CURRENT_DATE
          WHEN ss.day_of_week > EXTRACT(DOW FROM CURRENT_DATE) THEN
            CURRENT_DATE + (ss.day_of_week - EXTRACT(DOW FROM CURRENT_DATE))::INTEGER
          ELSE
            CURRENT_DATE + (7 - EXTRACT(DOW FROM CURRENT_DATE) + ss.day_of_week)::INTEGER
        END as show_date,
        1 as occurrence_number
      FROM show_schedules ss
      WHERE ss.show_id = #{showId}
        AND ss.is_active = true
        AND (ss.effective_until IS NULL OR ss.effective_until >= CURRENT_DATE)

      UNION ALL

      -- Recursive case: Generate next week's occurrences
      SELECT
        show_id,
        day_of_week,
        start_time,
        end_time,
        timezone,
        show_date + 7 as show_date,
        occurrence_number + 1
      FROM upcoming_dates
      WHERE occurrence_number < #{limit} * 2  -- Generate extra to ensure we have enough future dates
    )
    SELECT DISTINCT
      show_id,
      show_date,
      day_of_week,
      -- Convert date + time string to timestamptz in the show's timezone
      (show_date::TEXT || ' ' || start_time)::TIMESTAMP AT TIME ZONE timezone as start_time,
      (show_date::TEXT || ' ' || end_time)::TIMESTAMP AT TIME ZONE timezone as end_time
    FROM upcoming_dates
    WHERE show_date >= CURRENT_DATE
    ORDER BY show_date
    LIMIT #{limit}
  |]

-- | Get the next N upcoming UNscheduled dates for a specific show
-- Like getUpcomingShowDates, but filters out dates that already have episodes scheduled
-- This is used in the episode upload form to prevent double-booking time slots
getUpcomingUnscheduledShowDates :: Shows.Id -> Int64 -> Hasql.Statement () [UpcomingShowDate]
getUpcomingUnscheduledShowDates showId limit =
  fmap fromUpcomingShowDateRow
    <$> interp
      False
      [sql|
    WITH RECURSIVE upcoming_dates AS (
      -- Base case: Get all active schedules for this show
      SELECT
        ss.show_id,
        ss.day_of_week,
        ss.start_time,
        ss.end_time,
        ss.timezone,
        -- Calculate next occurrence of this day of week from today
        CASE
          WHEN ss.day_of_week = EXTRACT(DOW FROM CURRENT_DATE) THEN CURRENT_DATE
          WHEN ss.day_of_week > EXTRACT(DOW FROM CURRENT_DATE) THEN
            CURRENT_DATE + (ss.day_of_week - EXTRACT(DOW FROM CURRENT_DATE))::INTEGER
          ELSE
            CURRENT_DATE + (7 - EXTRACT(DOW FROM CURRENT_DATE) + ss.day_of_week)::INTEGER
        END as show_date,
        1 as occurrence_number
      FROM show_schedules ss
      WHERE ss.show_id = #{showId}
        AND ss.is_active = true
        AND (ss.effective_until IS NULL OR ss.effective_until >= CURRENT_DATE)

      UNION ALL

      -- Recursive case: Generate next week's occurrences
      SELECT
        show_id,
        day_of_week,
        start_time,
        end_time,
        timezone,
        show_date + 7 as show_date,
        occurrence_number + 1
      FROM upcoming_dates
      WHERE occurrence_number < #{limit} * 4  -- Generate extra to ensure we have enough unscheduled dates
    ),
    scheduled_dates AS (
      SELECT DISTINCT
        ud.show_id,
        ud.show_date,
        ud.day_of_week,
        (ud.show_date::TEXT || ' ' || ud.start_time)::TIMESTAMP AT TIME ZONE ud.timezone as start_time,
        (ud.show_date::TEXT || ' ' || ud.end_time)::TIMESTAMP AT TIME ZONE ud.timezone as end_time
      FROM upcoming_dates ud
      LEFT JOIN episodes e ON e.show_id = ud.show_id
        AND e.scheduled_at IS NOT NULL
        AND e.scheduled_at = (ud.show_date::TEXT || ' ' || ud.start_time)::TIMESTAMP AT TIME ZONE ud.timezone
      WHERE ud.show_date >= CURRENT_DATE
        AND e.id IS NULL  -- Only include dates without scheduled episodes
    )
    SELECT show_id, show_date, day_of_week, start_time, end_time
    FROM scheduled_dates
    ORDER BY show_date
    LIMIT #{limit}
  |]
