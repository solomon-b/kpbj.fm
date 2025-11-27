{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.ShowSchedule where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, displayBuilder)
import Data.Time (Day, DayOfWeek (..), TimeOfDay, UTCTime)
import Data.Vector (Vector)
import Domain.Types.Limit (Limit)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Shows qualified as Shows
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.DayOfWeek ()
import OrphanInstances.TimeOfDay ()

--------------------------------------------------------------------------------
-- Schedule Template Types

newtype TemplateId = TemplateId Int64
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      ToJSON,
      FromJSON,
      Display,
      DecodeValue,
      EncodeValue
    )

-- | Schedule template model
--
-- Immutable schedule pattern that can be either:
-- - Recurring: has day_of_week and weeks_of_month
-- - One-time: both fields are NULL (uses validity dates to define specific date)
data ScheduleTemplate = ScheduleTemplate
  { id :: TemplateId,
    showId :: Shows.Id,
    dayOfWeek :: Maybe DayOfWeek,
    weeksOfMonth :: Maybe (Vector Int64),
    startTime :: TimeOfDay,
    endTime :: TimeOfDay,
    timezone :: Text,
    createdAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display ScheduleTemplate where
  displayBuilder _ = "ScheduleTemplate"

-- | Insert type for creating new schedule templates
data ScheduleTemplateInsert = ScheduleTemplateInsert
  { stiShowId :: Shows.Id,
    stiDayOfWeek :: Maybe DayOfWeek,
    stiWeeksOfMonth :: Maybe (Vector Int64),
    stiStartTime :: TimeOfDay,
    stiEndTime :: TimeOfDay,
    stiTimezone :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)

--------------------------------------------------------------------------------
-- Schedule Template Validity Types

newtype ValidityId = ValidityId Int64
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      ToJSON,
      FromJSON,
      Display,
      DecodeValue,
      EncodeValue
    )

-- | Schedule template validity model
--
-- Defines time-bounded periods when a schedule template is active
-- - effective_from: Inclusive start date
-- - effective_until: Exclusive end date (NULL = currently active)
data ScheduleTemplateValidity = ScheduleTemplateValidity
  { id :: ValidityId,
    templateId :: TemplateId,
    effectiveFrom :: Day,
    effectiveUntil :: Maybe Day
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display ScheduleTemplateValidity where
  displayBuilder _ = "ScheduleTemplateValidity"

-- | Insert type for creating new validity periods
data ValidityInsert = ValidityInsert
  { viTemplateId :: TemplateId,
    viEffectiveFrom :: Day,
    viEffectiveUntil :: Maybe Day
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)

-- | Update type for modifying validity periods
data ValidityUpdate = ValidityUpdate
  { vuEffectiveFrom :: Day,
    vuEffectiveUntil :: Maybe Day
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)

--------------------------------------------------------------------------------
-- Schedule Template Queries

-- | Get a schedule template by ID
getScheduleTemplateById :: TemplateId -> Hasql.Statement () (Maybe ScheduleTemplate)
getScheduleTemplateById templateId =
  interp
    False
    [sql|
    SELECT id, show_id, day_of_week, weeks_of_month, start_time, end_time, timezone, created_at
    FROM schedule_templates
    WHERE id = #{templateId}
  |]

-- | Get all schedule templates for a show
getScheduleTemplatesForShow :: Shows.Id -> Hasql.Statement () [ScheduleTemplate]
getScheduleTemplatesForShow showId =
  interp
    False
    [sql|
    SELECT st.id, st.show_id, st.day_of_week, st.weeks_of_month, st.start_time, st.end_time, st.timezone, st.created_at
    FROM schedule_templates st
    WHERE st.show_id = #{showId}
    ORDER BY st.day_of_week, st.start_time
  |]

-- | Get currently active schedule templates for a show
--
-- Joins with schedule_template_validity to only return templates with active validity periods
getActiveScheduleTemplatesForShow :: Shows.Id -> Hasql.Statement () [ScheduleTemplate]
getActiveScheduleTemplatesForShow showId =
  interp
    False
    [sql|
    SELECT DISTINCT st.id, st.show_id, st.day_of_week, st.weeks_of_month, st.start_time, st.end_time, st.timezone, st.created_at
    FROM schedule_templates st
    JOIN schedule_template_validity stv ON stv.template_id = st.id
    WHERE st.show_id = #{showId}
      AND stv.effective_from <= CURRENT_DATE
      AND (stv.effective_until IS NULL OR stv.effective_until > CURRENT_DATE)
    ORDER BY st.day_of_week, st.start_time
  |]

-- | Get all currently active recurring schedule templates across all shows
--
-- Filters out one-time shows (where day_of_week IS NULL)
getActiveRecurringScheduleTemplates :: Hasql.Statement () [ScheduleTemplate]
getActiveRecurringScheduleTemplates =
  interp
    False
    [sql|
    SELECT DISTINCT st.id, st.show_id, st.day_of_week, st.weeks_of_month, st.start_time, st.end_time, st.timezone, st.created_at
    FROM schedule_templates st
    JOIN schedule_template_validity stv ON stv.template_id = st.id
    JOIN shows s ON s.id = st.show_id
    WHERE s.status = 'active'
      AND st.day_of_week IS NOT NULL
      AND stv.effective_from <= CURRENT_DATE
      AND (stv.effective_until IS NULL OR stv.effective_until > CURRENT_DATE)
    ORDER BY st.day_of_week, st.start_time
  |]

-- | Insert a new schedule template
--
-- Returns the generated ID
insertScheduleTemplate :: ScheduleTemplateInsert -> Hasql.Statement () TemplateId
insertScheduleTemplate ScheduleTemplateInsert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO schedule_templates(show_id, day_of_week, weeks_of_month, start_time, end_time, timezone, created_at)
    VALUES (#{stiShowId}, #{stiDayOfWeek}::day_of_week, #{stiWeeksOfMonth}, #{stiStartTime}, #{stiEndTime}, #{stiTimezone}, NOW())
    RETURNING id
  |]

-- | Delete a schedule template
--
-- CASCADE will automatically delete related validity records
deleteScheduleTemplate :: TemplateId -> Hasql.Statement () (Maybe TemplateId)
deleteScheduleTemplate templateId =
  interp
    False
    [sql|
    DELETE FROM schedule_templates
    WHERE id = #{templateId}
    RETURNING id
  |]

--------------------------------------------------------------------------------
-- Schedule Template Validity Queries

-- | Get a validity period by ID
getValidityById :: ValidityId -> Hasql.Statement () (Maybe ScheduleTemplateValidity)
getValidityById validityId =
  interp
    False
    [sql|
    SELECT id, template_id, effective_from, effective_until
    FROM schedule_template_validity
    WHERE id = #{validityId}
  |]

-- | Get all validity periods for a template
getValidityPeriodsForTemplate :: TemplateId -> Hasql.Statement () [ScheduleTemplateValidity]
getValidityPeriodsForTemplate templateId =
  interp
    False
    [sql|
    SELECT id, template_id, effective_from, effective_until
    FROM schedule_template_validity
    WHERE template_id = #{templateId}
    ORDER BY effective_from DESC
  |]

-- | Get currently active validity periods for a template
getActiveValidityPeriodsForTemplate :: TemplateId -> Hasql.Statement () [ScheduleTemplateValidity]
getActiveValidityPeriodsForTemplate templateId =
  interp
    False
    [sql|
    SELECT id, template_id, effective_from, effective_until
    FROM schedule_template_validity
    WHERE template_id = #{templateId}
      AND effective_from <= CURRENT_DATE
      AND (effective_until IS NULL OR effective_until > CURRENT_DATE)
    ORDER BY effective_from DESC
  |]

-- | Insert a new validity period
--
-- Returns the generated ID
insertValidity :: ValidityInsert -> Hasql.Statement () ValidityId
insertValidity ValidityInsert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO schedule_template_validity(template_id, effective_from, effective_until)
    VALUES (#{viTemplateId}, #{viEffectiveFrom}, #{viEffectiveUntil})
    RETURNING id
  |]

-- | Update a validity period
--
-- Used to adjust the time bounds of when a template is active
updateValidity :: ValidityId -> ValidityUpdate -> Hasql.Statement () (Maybe ValidityId)
updateValidity validityId ValidityUpdate {..} =
  interp
    False
    [sql|
    UPDATE schedule_template_validity
    SET effective_from = #{vuEffectiveFrom}, effective_until = #{vuEffectiveUntil}
    WHERE id = #{validityId}
    RETURNING id
  |]

-- | Delete a validity period
deleteValidity :: ValidityId -> Hasql.Statement () (Maybe ValidityId)
deleteValidity validityId =
  interp
    False
    [sql|
    DELETE FROM schedule_template_validity
    WHERE id = #{validityId}
    RETURNING id
  |]

-- | End a validity period by setting effective_until to a specific date
--
-- Used to "close" a validity period when a schedule changes
endValidity :: ValidityId -> Day -> Hasql.Statement () (Maybe ValidityId)
endValidity validityId endDate =
  interp
    False
    [sql|
    UPDATE schedule_template_validity
    SET effective_until = #{endDate}
    WHERE id = #{validityId}
    RETURNING id
  |]

--------------------------------------------------------------------------------
-- Scheduled Show With Details (for schedule views)

-- | Combined view of schedule templates with show and host information
--
-- Used for rendering schedule grids and calendars
data ScheduledShowWithDetails = ScheduledShowWithDetails
  { sswdDate :: Day,
    sswdDayOfWeek :: DayOfWeek,
    sswdStartTime :: TimeOfDay,
    sswdEndTime :: TimeOfDay,
    sswdShowSlug :: Slug,
    sswdShowTitle :: Text,
    sswdHostName :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON, DecodeRow)

instance Display ScheduledShowWithDetails where
  displayBuilder _ = "ScheduledShowWithDetails"

-- | Get all scheduled shows for a specific date with show and host details
--
-- Returns both recurring and one-time shows scheduled for the given date.
-- Used for rendering actual weekly schedules (not just templates).
getScheduledShowsForDate :: Day -> Hasql.Statement () [ScheduledShowWithDetails]
getScheduledShowsForDate targetDate =
  interp
    False
    [sql|
    SELECT DISTINCT
      #{targetDate}::date as show_date,
      COALESCE(
        st.day_of_week,
        CASE EXTRACT(DOW FROM #{targetDate}::date)::INTEGER
          WHEN 0 THEN 'sunday'::day_of_week
          WHEN 1 THEN 'monday'::day_of_week
          WHEN 2 THEN 'tuesday'::day_of_week
          WHEN 3 THEN 'wednesday'::day_of_week
          WHEN 4 THEN 'thursday'::day_of_week
          WHEN 5 THEN 'friday'::day_of_week
          WHEN 6 THEN 'saturday'::day_of_week
        END
      ) as day_of_week,
      st.start_time::time,
      st.end_time::time,
      s.slug,
      s.title,
      COALESCE(um.display_name, um.full_name) as host_name
    FROM schedule_templates st
    JOIN schedule_template_validity stv ON stv.template_id = st.id
    JOIN shows s ON s.id = st.show_id
    LEFT JOIN show_hosts sh ON sh.show_id = s.id AND sh.left_at IS NULL AND sh.is_primary = TRUE
    LEFT JOIN users u ON u.id = sh.user_id
    LEFT JOIN user_metadata um ON um.user_id = u.id
    WHERE s.status = 'active'
      AND stv.effective_from <= #{targetDate}::date
      AND (stv.effective_until IS NULL OR stv.effective_until > #{targetDate}::date)
      AND (
        -- Recurring shows: match day of week and week of month
        (st.day_of_week IS NOT NULL
         AND EXTRACT(DOW FROM #{targetDate}::date)::INTEGER =
             CASE st.day_of_week::TEXT
               WHEN 'sunday' THEN 0
               WHEN 'monday' THEN 1
               WHEN 'tuesday' THEN 2
               WHEN 'wednesday' THEN 3
               WHEN 'thursday' THEN 4
               WHEN 'friday' THEN 5
               WHEN 'saturday' THEN 6
             END
         AND (st.weeks_of_month IS NULL
              OR CEIL(EXTRACT(DAY FROM #{targetDate}::date) / 7.0)::INTEGER = ANY(st.weeks_of_month))
        )
        OR
        -- One-time shows: match exact date via validity period
        (st.day_of_week IS NULL
         AND stv.effective_from = #{targetDate}::date)
      )
    ORDER BY st.start_time
  |]

--------------------------------------------------------------------------------
-- Upcoming Show Dates (for episode scheduling)

-- | Data type to represent an upcoming show date
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

-- | Convert from database row to UpcomingShowDate
fromUpcomingShowDateRow :: (Shows.Id, Day, DayOfWeek, UTCTime, UTCTime) -> UpcomingShowDate
fromUpcomingShowDateRow (showId, showDate, dayOfWeek, startTime, endTime) =
  UpcomingShowDate
    { usdId = showId,
      usdShowDate = showDate,
      usdDayOfWeek = dayOfWeek,
      usdStartTime = startTime,
      usdEndTime = endTime
    }

-- | Get the next N upcoming scheduled dates for a specific show
--
-- This generates dates based on the show's schedule templates and validity periods.
-- For N-of-month schedules, it calculates which weeks of each month the show airs.
getUpcomingShowDates :: Shows.Id -> Day -> Limit -> Hasql.Statement () [UpcomingShowDate]
getUpcomingShowDates showId referenceDate limit =
  fmap fromUpcomingShowDateRow
    <$> interp
      False
      [sql|
    WITH RECURSIVE date_series AS (
      -- Generate a series of dates starting from the reference date
      SELECT
        #{referenceDate}::DATE as date,
        1 as n
      UNION ALL
      SELECT
        date + 1,
        n + 1
      FROM date_series
      WHERE n < 365  -- Look ahead up to a year
    ),
    schedule_instances AS (
      -- For each active template, find matching dates
      SELECT DISTINCT
        st.show_id,
        ds.date as show_date,
        st.day_of_week,
        st.start_time,
        st.end_time,
        st.timezone
      FROM schedule_templates st
      JOIN schedule_template_validity stv ON stv.template_id = st.id
      CROSS JOIN date_series ds
      WHERE st.show_id = #{showId}
        AND st.day_of_week IS NOT NULL  -- Only recurring shows
        AND stv.effective_from <= ds.date
        AND (stv.effective_until IS NULL OR stv.effective_until > ds.date)
        AND EXTRACT(DOW FROM ds.date)::INTEGER =
            CASE st.day_of_week::TEXT
              WHEN 'sunday' THEN 0
              WHEN 'monday' THEN 1
              WHEN 'tuesday' THEN 2
              WHEN 'wednesday' THEN 3
              WHEN 'thursday' THEN 4
              WHEN 'friday' THEN 5
              WHEN 'saturday' THEN 6
            END
        AND (
          st.weeks_of_month IS NULL OR
          -- Check if current week of month is in the array
          CEIL(EXTRACT(DAY FROM ds.date) / 7.0)::INTEGER = ANY(st.weeks_of_month)
        )
    )
    SELECT
      show_id,
      show_date,
      day_of_week,
      (show_date::TEXT || ' ' || start_time::TEXT)::TIMESTAMP AT TIME ZONE timezone as start_time,
      (show_date::TEXT || ' ' || end_time::TEXT)::TIMESTAMP AT TIME ZONE timezone as end_time
    FROM schedule_instances
    WHERE show_date >= #{referenceDate}::DATE
    ORDER BY show_date
    LIMIT #{limit}
  |]

-- | Get the next N upcoming UNscheduled dates for a specific show
--
-- Like getUpcomingShowDates, but filters out dates that already have episodes scheduled.
-- This is used in the episode upload form to prevent double-booking time slots.
getUpcomingUnscheduledShowDates :: Shows.Id -> Limit -> Hasql.Statement () [UpcomingShowDate]
getUpcomingUnscheduledShowDates showId limit =
  fmap fromUpcomingShowDateRow
    <$> interp
      False
      [sql|
    WITH RECURSIVE date_series AS (
      SELECT
        CURRENT_DATE as date,
        1 as n
      UNION ALL
      SELECT
        date + 1,
        n + 1
      FROM date_series
      WHERE n < 365
    ),
    schedule_instances AS (
      SELECT DISTINCT
        st.show_id,
        ds.date as show_date,
        st.day_of_week,
        st.start_time,
        st.end_time,
        st.timezone
      FROM schedule_templates st
      JOIN schedule_template_validity stv ON stv.template_id = st.id
      CROSS JOIN date_series ds
      WHERE st.show_id = #{showId}
        AND st.day_of_week IS NOT NULL
        AND stv.effective_from <= ds.date
        AND (stv.effective_until IS NULL OR stv.effective_until > ds.date)
        AND EXTRACT(DOW FROM ds.date)::INTEGER =
            CASE st.day_of_week::TEXT
              WHEN 'sunday' THEN 0
              WHEN 'monday' THEN 1
              WHEN 'tuesday' THEN 2
              WHEN 'wednesday' THEN 3
              WHEN 'thursday' THEN 4
              WHEN 'friday' THEN 5
              WHEN 'saturday' THEN 6
            END
        AND (
          st.weeks_of_month IS NULL OR
          CEIL(EXTRACT(DAY FROM ds.date) / 7.0)::INTEGER = ANY(st.weeks_of_month)
        )
    ),
    unscheduled_instances AS (
      SELECT
        si.show_id,
        si.show_date,
        si.day_of_week,
        (si.show_date::TEXT || ' ' || si.start_time::TEXT)::TIMESTAMP AT TIME ZONE si.timezone as start_time,
        (si.show_date::TEXT || ' ' || si.end_time::TEXT)::TIMESTAMP AT TIME ZONE si.timezone as end_time
      FROM schedule_instances si
      LEFT JOIN episodes e ON e.show_id = si.show_id
        AND e.scheduled_at = (si.show_date::TEXT || ' ' || si.start_time::TEXT)::TIMESTAMP AT TIME ZONE si.timezone
      WHERE e.id IS NULL  -- Only dates without scheduled episodes
        AND si.show_date >= CURRENT_DATE
    )
    SELECT show_id, show_date, day_of_week, start_time, end_time
    FROM unscheduled_instances
    ORDER BY show_date
    LIMIT #{limit}
  |]
