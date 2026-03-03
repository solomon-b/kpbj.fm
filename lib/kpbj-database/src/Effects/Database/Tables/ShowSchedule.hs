{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definitions and queries for @schedule_templates@ and @schedule_template_validity@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex queries
-- involving CTEs, date arithmetic, and PostgreSQL-specific functions.
module Effects.Database.Tables.ShowSchedule
  ( -- * Schedule Template Types
    TemplateId (..),
    ScheduleTemplate (..),
    scheduleTemplateSchema,
    ScheduleTemplateInsert (..),

    -- * Schedule Template Validity Types
    ValidityId (..),
    ScheduleTemplateValidity (..),
    ValidityInsert (..),

    -- * Schedule Template Queries
    getScheduleTemplateById,
    getScheduleTemplatesForShow,
    getActiveScheduleTemplatesForShow,
    checkTimeSlotConflict,
    insertScheduleTemplate,

    -- * Schedule Template Validity Queries
    getActiveValidityPeriodsForTemplate,
    insertValidity,
    endValidity,

    -- * Scheduled Show With Details
    ScheduledShowWithDetails (..),
    getScheduledShowsForDate,

    -- * Upcoming Show Dates
    UpcomingShowDate (..),
    getUpcomingShowDates,
    getUpcomingUnscheduledShowDates,
    makeUpcomingShowDateFromTemplate,

    -- * Missing Episodes
    ShowMissingEpisode (..),
    getShowsMissingEpisodes,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Time (Day, DayOfWeek (..), LocalTime (..), TimeOfDay, UTCTime, addUTCTime, dayOfWeek)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (timeOfDayToTime)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Slug (Slug)
import Domain.Types.Timezone (utcToPacific)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Data.Text.Display (display)
import OrphanInstances.DayOfWeek ()
import OrphanInstances.Rel8 ()
import OrphanInstances.TimeOfDay ()
import Rel8 hiding (Insert)
import Rel8 qualified

--------------------------------------------------------------------------------
-- Schedule Template Types

-- | Newtype wrapper for schedule template primary keys.
newtype TemplateId = TemplateId {unTemplateId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq)
  deriving newtype (ToJSON, FromJSON, Display, DecodeValue, EncodeValue)

-- | The @schedule_templates@ table definition using rel8's higher-kinded data pattern.
--
-- Schedule template model - Immutable schedule pattern that can be either:
--
-- - Recurring: has day_of_week and weeks_of_month
-- - One-time: both fields are NULL (uses validity dates to define specific date)
data ScheduleTemplate f = ScheduleTemplate
  { stId :: Column f TemplateId,
    stShowId :: Column f Shows.Id,
    stDayOfWeek :: Column f (Maybe DayOfWeek),
    stWeeksOfMonth :: Column f (Maybe [Int64]),
    stStartTime :: Column f TimeOfDay,
    stEndTime :: Column f TimeOfDay,
    stTimezone :: Column f Text,
    stCreatedAt :: Column f UTCTime,
    stAirsTwiceDaily :: Column f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (ScheduleTemplate f)

deriving stock instance (f ~ Result) => Eq (ScheduleTemplate f)

instance DecodeRow (ScheduleTemplate Result)

instance Display (ScheduleTemplate Result) where
  displayBuilder _ = "ScheduleTemplate"

-- | Table schema for schedule_templates.
scheduleTemplateSchema :: TableSchema (ScheduleTemplate Name)
scheduleTemplateSchema =
  TableSchema
    { name = "schedule_templates",
      columns =
        ScheduleTemplate
          { stId = "id",
            stShowId = "show_id",
            stDayOfWeek = "day_of_week",
            stWeeksOfMonth = "weeks_of_month",
            stStartTime = "start_time",
            stEndTime = "end_time",
            stTimezone = "timezone",
            stCreatedAt = "created_at",
            stAirsTwiceDaily = "airs_twice_daily"
          }
    }

-- | Insert type for creating new schedule templates.
data ScheduleTemplateInsert = ScheduleTemplateInsert
  { stiShowId :: Shows.Id,
    stiDayOfWeek :: Maybe DayOfWeek,
    stiWeeksOfMonth :: Maybe [Int64],
    stiStartTime :: TimeOfDay,
    stiEndTime :: TimeOfDay,
    stiTimezone :: Text,
    stiAirsTwiceDaily :: Bool
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Schedule Template Validity Types

-- | Newtype wrapper for schedule template validity primary keys.
newtype ValidityId = ValidityId {unValidityId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq)
  deriving newtype (ToJSON, FromJSON, Display, DecodeValue, EncodeValue)

-- | The @schedule_template_validity@ table definition using rel8's higher-kinded data pattern.
--
-- Defines time-bounded periods when a schedule template is active:
--
-- - effective_from: Inclusive start date
-- - effective_until: Exclusive end date (NULL = currently active)
data ScheduleTemplateValidity f = ScheduleTemplateValidity
  { stvId :: Column f ValidityId,
    stvTemplateId :: Column f TemplateId,
    stvEffectiveFrom :: Column f Day,
    stvEffectiveUntil :: Column f (Maybe Day)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (ScheduleTemplateValidity f)

deriving stock instance (f ~ Result) => Eq (ScheduleTemplateValidity f)

instance DecodeRow (ScheduleTemplateValidity Result)

instance Display (ScheduleTemplateValidity Result) where
  displayBuilder _ = "ScheduleTemplateValidity"

-- | Table schema for schedule_template_validity.
scheduleTemplateValiditySchema :: TableSchema (ScheduleTemplateValidity Name)
scheduleTemplateValiditySchema =
  TableSchema
    { name = "schedule_template_validity",
      columns =
        ScheduleTemplateValidity
          { stvId = "id",
            stvTemplateId = "template_id",
            stvEffectiveFrom = "effective_from",
            stvEffectiveUntil = "effective_until"
          }
    }

-- | Insert type for creating new validity periods.
data ValidityInsert = ValidityInsert
  { viTemplateId :: TemplateId,
    viEffectiveFrom :: Day,
    viEffectiveUntil :: Maybe Day
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Schedule Template Queries

-- | Get a schedule template by ID.
getScheduleTemplateById :: TemplateId -> Hasql.Statement () (Maybe (ScheduleTemplate Result))
getScheduleTemplateById templateId = fmap listToMaybe $ run $ select do
  st <- each scheduleTemplateSchema
  where_ $ stId st ==. lit templateId
  pure st

-- | Get all schedule templates for a show.
getScheduleTemplatesForShow :: Shows.Id -> Hasql.Statement () [ScheduleTemplate Result]
getScheduleTemplatesForShow showId =
  run $
    select $
      orderBy ((stDayOfWeek >$< nullsLast asc) <> (stStartTime >$< asc)) do
        st <- each scheduleTemplateSchema
        where_ $ stShowId st ==. lit showId
        pure st

-- | Get currently active schedule templates for a show.
--
-- Joins with schedule_template_validity to only return templates with active validity periods.
-- Uses raw SQL because of CURRENT_DATE and join conditions.
getActiveScheduleTemplatesForShow :: Shows.Id -> Hasql.Statement () [ScheduleTemplate Result]
getActiveScheduleTemplatesForShow showId =
  interp
    False
    [sql|
    SELECT DISTINCT st.id, st.show_id, st.day_of_week, st.weeks_of_month, st.start_time, st.end_time, st.timezone, st.created_at, st.airs_twice_daily
    FROM schedule_templates st
    JOIN schedule_template_validity stv ON stv.template_id = st.id
    WHERE st.show_id = #{showId}
      AND stv.effective_from <= CURRENT_DATE
      AND (stv.effective_until IS NULL OR stv.effective_until > CURRENT_DATE)
    ORDER BY st.day_of_week, st.start_time
  |]

-- | Wrapper for single Text result from conflict check.
newtype ConflictingShowTitle = ConflictingShowTitle {getConflictingShowTitle :: Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

-- | Check if a time slot conflicts with any active schedule (excluding a specific show).
--
-- Returns the title of the conflicting show if there's a conflict, Nothing otherwise.
-- Checks for overlapping time ranges on the same day of week with overlapping weeks.
-- Uses raw SQL because of complex overlap logic and array operations.
-- Excludes soft-deleted shows.
checkTimeSlotConflict ::
  Shows.Id ->
  DayOfWeek ->
  [Int64] ->
  TimeOfDay ->
  TimeOfDay ->
  Hasql.Statement () (Maybe Text)
checkTimeSlotConflict excludeShowId dow weeks start end =
  fmap getConflictingShowTitle
    <$> interp
      False
      [sql|
    SELECT s.title
    FROM schedule_templates st
    JOIN schedule_template_validity stv ON stv.template_id = st.id
    JOIN shows s ON s.id = st.show_id
    WHERE s.status = 'active'
      AND s.deleted_at IS NULL
      AND st.show_id != #{excludeShowId}
      AND st.day_of_week = #{dow}::day_of_week
      AND stv.effective_from <= CURRENT_DATE
      AND (stv.effective_until IS NULL OR stv.effective_until > CURRENT_DATE)
      -- Check weeks overlap
      AND (st.weeks_of_month IS NULL OR st.weeks_of_month && #{weeks})
      -- Check time overlap (handles overnight shows)
      AND (
        CASE
          -- Neither overnight: simple overlap check
          WHEN st.end_time > st.start_time AND #{end} > #{start} THEN
            st.start_time < #{end} AND #{start} < st.end_time
          -- Existing is overnight, new is not
          WHEN st.end_time <= st.start_time AND #{end} > #{start} THEN
            #{start} < st.end_time OR #{end} > st.start_time
          -- New is overnight, existing is not
          WHEN #{end} <= #{start} AND st.end_time > st.start_time THEN
            st.start_time < #{end} OR st.end_time > #{start}
          -- Both overnight: always overlap
          ELSE TRUE
        END
      )
    LIMIT 1
  |]

-- | Insert a new schedule template.
--
-- Returns the generated ID.
-- Uses raw SQL because of NOW() and enum cast.
insertScheduleTemplate :: ScheduleTemplateInsert -> Hasql.Statement () TemplateId
insertScheduleTemplate ScheduleTemplateInsert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO schedule_templates(show_id, day_of_week, weeks_of_month, start_time, end_time, timezone, created_at, airs_twice_daily)
    VALUES (#{stiShowId}, #{stiDayOfWeek}::day_of_week, #{stiWeeksOfMonth}, #{stiStartTime}, #{stiEndTime}, #{stiTimezone}, NOW(), #{stiAirsTwiceDaily})
    RETURNING id
  |]

--------------------------------------------------------------------------------
-- Schedule Template Validity Queries

-- | Get currently active validity periods for a template.
--
-- Uses raw SQL because of CURRENT_DATE comparisons.
getActiveValidityPeriodsForTemplate :: TemplateId -> Hasql.Statement () [ScheduleTemplateValidity Result]
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

-- | Insert a new validity period.
--
-- Returns the generated ID.
insertValidity :: ValidityInsert -> Hasql.Statement () (Maybe ValidityId)
insertValidity ValidityInsert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = scheduleTemplateValiditySchema,
            rows =
              values
                [ ScheduleTemplateValidity
                    { stvId = nextId "schedule_template_validity_id_seq",
                      stvTemplateId = lit viTemplateId,
                      stvEffectiveFrom = lit viEffectiveFrom,
                      stvEffectiveUntil = lit viEffectiveUntil
                    }
                ],
            onConflict = Abort,
            returning = Returning stvId
          }

-- | End a validity period by setting effective_until to a specific date.
--
-- Used to "close" a validity period when a schedule changes.
endValidity :: ValidityId -> Day -> Hasql.Statement () (Maybe ValidityId)
endValidity validityId endDate =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = scheduleTemplateValiditySchema,
            from = pure (),
            set = \_ validity ->
              validity
                { stvEffectiveUntil = lit (Just endDate)
                },
            updateWhere = \_ validity -> stvId validity ==. lit validityId,
            returning = Returning stvId
          }

--------------------------------------------------------------------------------
-- Scheduled Show With Details (for schedule views)

-- | Combined view of schedule templates with show and host information.
--
-- Used for rendering schedule grids and calendars.
data ScheduledShowWithDetails = ScheduledShowWithDetails
  { sswdDate :: Day,
    sswdDayOfWeek :: DayOfWeek,
    sswdStartTime :: TimeOfDay,
    sswdEndTime :: TimeOfDay,
    sswdShowSlug :: Slug,
    sswdShowTitle :: Text,
    sswdHostName :: Text,
    sswdLogoUrl :: Maybe Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON, DecodeRow)

instance Display ScheduledShowWithDetails where
  displayBuilder _ = "ScheduledShowWithDetails"

-- | Get all scheduled shows for a specific date with show and host details.
--
-- Returns both recurring and one-time shows scheduled for the given date.
-- For shows with airs_twice_daily = TRUE, returns two rows (primary and +12 hours).
-- Used for rendering actual weekly schedules (not just templates).
-- Uses raw SQL because of complex date arithmetic and CASE expressions.
-- Excludes soft-deleted shows.
getScheduledShowsForDate :: Day -> Hasql.Statement () [ScheduledShowWithDetails]
getScheduledShowsForDate targetDate =
  interp
    False
    [sql|
    -- Primary airings (all shows)
    SELECT
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
      COALESCE(
        STRING_AGG(COALESCE(um.display_name, um.full_name), ', ' ORDER BY sh.joined_at),
        'TBD'
      ) as host_names,
      s.logo_url
    FROM schedule_templates st
    JOIN schedule_template_validity stv ON stv.template_id = st.id
    JOIN shows s ON s.id = st.show_id
    LEFT JOIN show_hosts sh ON sh.show_id = s.id AND sh.left_at IS NULL
    LEFT JOIN users u ON u.id = sh.user_id
    LEFT JOIN user_metadata um ON um.user_id = u.id
    WHERE s.status = 'active'
      AND s.deleted_at IS NULL
      AND EXISTS (SELECT 1 FROM show_hosts sh2 WHERE sh2.show_id = s.id AND sh2.left_at IS NULL)
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
    GROUP BY st.id, st.day_of_week, st.start_time, st.end_time, s.slug, s.title, s.logo_url

    UNION ALL

    -- Replay airings (+12 hours) for dual-airing shows
    SELECT
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
      ((st.start_time + INTERVAL '12 hours')::time) as start_time,
      ((st.end_time + INTERVAL '12 hours')::time) as end_time,
      s.slug,
      s.title,
      COALESCE(
        STRING_AGG(COALESCE(um.display_name, um.full_name), ', ' ORDER BY sh.joined_at),
        'TBD'
      ) as host_names,
      s.logo_url
    FROM schedule_templates st
    JOIN schedule_template_validity stv ON stv.template_id = st.id
    JOIN shows s ON s.id = st.show_id
    LEFT JOIN show_hosts sh ON sh.show_id = s.id AND sh.left_at IS NULL
    LEFT JOIN users u ON u.id = sh.user_id
    LEFT JOIN user_metadata um ON um.user_id = u.id
    WHERE s.status = 'active'
      AND s.deleted_at IS NULL
      AND EXISTS (SELECT 1 FROM show_hosts sh2 WHERE sh2.show_id = s.id AND sh2.left_at IS NULL)
      AND st.airs_twice_daily = TRUE
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
    GROUP BY st.id, st.day_of_week, st.start_time, st.end_time, s.slug, s.title, s.logo_url

    ORDER BY start_time
  |]

--------------------------------------------------------------------------------
-- Upcoming Show Dates (for episode scheduling)

-- | Data type to represent an upcoming show date.
data UpcomingShowDate = UpcomingShowDate
  { usdId :: Shows.Id,
    usdTemplateId :: TemplateId,
    usdShowDate :: Day,
    usdDayOfWeek :: DayOfWeek,
    usdStartTime :: UTCTime,
    usdEndTime :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

instance Display UpcomingShowDate where
  displayBuilder usd =
    let startTimePacific = utcToPacific (usdStartTime usd)
        endTimePacific = utcToPacific (usdEndTime usd)
        formatTimeOfDay = Text.pack . formatTime defaultTimeLocale "%l:%M %p"
     in displayBuilder $
          display (usdDayOfWeek usd)
            <> ", "
            <> Text.pack (show (usdShowDate usd))
            <> " ("
            <> formatTimeOfDay startTimePacific
            <> " - "
            <> formatTimeOfDay endTimePacific
            <> " PT)"

-- | Convert from database row to UpcomingShowDate.
fromUpcomingShowDateRow :: (Shows.Id, TemplateId, Day, DayOfWeek, UTCTime, UTCTime) -> UpcomingShowDate
fromUpcomingShowDateRow (showId, templateId, showDate, dow, startTime, endTime) =
  UpcomingShowDate
    { usdId = showId,
      usdTemplateId = templateId,
      usdShowDate = showDate,
      usdDayOfWeek = dow,
      usdStartTime = startTime,
      usdEndTime = endTime
    }

-- | Get the next N upcoming scheduled dates for a specific show.
--
-- This generates dates based on the show's schedule templates and validity periods.
-- For N-of-month schedules, it calculates which weeks of each month the show airs.
-- Uses raw SQL because of recursive CTEs and complex date arithmetic.
getUpcomingShowDates :: Shows.Id -> Day -> Limit -> Hasql.Statement () [UpcomingShowDate]
getUpcomingShowDates showId referenceDate (Limit limitVal) =
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
        st.id as template_id,
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
      template_id,
      show_date,
      day_of_week,
      (show_date::TEXT || ' ' || start_time::TEXT)::TIMESTAMP AT TIME ZONE timezone as start_time,
      -- If end_time <= start_time, it's an overnight show (e.g., 23:00-00:00)
      -- In that case, end_time belongs to the next day
      (CASE WHEN end_time <= start_time
        THEN ((show_date + INTERVAL '1 day')::DATE::TEXT || ' ' || end_time::TEXT)
        ELSE (show_date::TEXT || ' ' || end_time::TEXT)
      END)::TIMESTAMP AT TIME ZONE timezone as end_time
    FROM schedule_instances
    WHERE show_date >= #{referenceDate}::DATE
    ORDER BY show_date
    LIMIT #{limitVal}
  |]

-- | Get the next N upcoming UNscheduled dates for a specific show.
--
-- Like getUpcomingShowDates, but filters out dates that already have episodes scheduled.
-- This is used in the episode upload form to prevent double-booking time slots.
-- Uses raw SQL because of recursive CTEs and complex date arithmetic.
getUpcomingUnscheduledShowDates :: Shows.Id -> Limit -> Hasql.Statement () [UpcomingShowDate]
getUpcomingUnscheduledShowDates showId (Limit limitVal) =
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
        st.id as template_id,
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
        si.template_id,
        si.show_date,
        si.day_of_week,
        (si.show_date::TEXT || ' ' || si.start_time::TEXT)::TIMESTAMP AT TIME ZONE si.timezone as start_time,
        -- If end_time <= start_time, it's an overnight show (e.g., 23:00-00:00)
        -- In that case, end_time belongs to the next day
        (CASE WHEN si.end_time <= si.start_time
          THEN ((si.show_date + INTERVAL '1 day')::DATE::TEXT || ' ' || si.end_time::TEXT)
          ELSE (si.show_date::TEXT || ' ' || si.end_time::TEXT)
        END)::TIMESTAMP AT TIME ZONE si.timezone as end_time
      FROM schedule_instances si
      LEFT JOIN episodes e ON e.show_id = si.show_id
        AND e.scheduled_at = (si.show_date::TEXT || ' ' || si.start_time::TEXT)::TIMESTAMP AT TIME ZONE si.timezone
      WHERE e.id IS NULL  -- Only dates without scheduled episodes
        AND si.show_date >= CURRENT_DATE
    )
    SELECT show_id, template_id, show_date, day_of_week, start_time, end_time
    FROM unscheduled_instances
    ORDER BY show_date
    LIMIT #{limitVal}
  |]

--------------------------------------------------------------------------------
-- Helper Functions

-- | Construct an UpcomingShowDate from a schedule template and scheduled time.
--
-- This is used to render the current episode's schedule slot in the same format
-- as the upcoming available slots. The scheduled_at timestamp from the episode
-- is used as the start time, and the end time is calculated from the template's
-- duration.
makeUpcomingShowDateFromTemplate ::
  -- | The schedule template
  ScheduleTemplate Result ->
  -- | The scheduled start time (from episode)
  UTCTime ->
  UpcomingShowDate
makeUpcomingShowDateFromTemplate template scheduledAt =
  let -- Convert to Pacific time to get the correct local date
      pacificTime = utcToPacific scheduledAt
      pacificDay = localDay pacificTime
   in UpcomingShowDate
        { usdId = template.stShowId,
          usdTemplateId = template.stId,
          usdShowDate = pacificDay,
          usdDayOfWeek = fromMaybe (dayOfWeek pacificDay) template.stDayOfWeek,
          usdStartTime = scheduledAt,
          usdEndTime = computeEndTime template scheduledAt
        }
  where
    -- Compute end time by adding the show duration to the start time
    computeEndTime :: ScheduleTemplate Result -> UTCTime -> UTCTime
    computeEndTime tmpl startTime =
      let startTod = tmpl.stStartTime
          endTod = tmpl.stEndTime
          -- Duration in seconds, handling overnight shows
          durationSecs =
            if endTod > startTod
              then timeOfDayToTime endTod - timeOfDayToTime startTod
              else -- Overnight show: add 24 hours worth of seconds
                (24 * 60 * 60) - timeOfDayToTime startTod + timeOfDayToTime endTod
       in addUTCTime (realToFrac durationSecs) startTime

--------------------------------------------------------------------------------
-- Missing Episodes

-- | A show scheduled within the next 7 days that is missing an episode upload.
data ShowMissingEpisode = ShowMissingEpisode
  { smeShowId :: Shows.Id,
    smeShowTitle :: Text,
    smeShowSlug :: Slug,
    smeHostNames :: Text,
    smeShowDate :: Day,
    smeDayOfWeek :: DayOfWeek,
    smeStartTime :: TimeOfDay,
    smeEndTime :: TimeOfDay
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (DecodeRow)

instance Display ShowMissingEpisode where
  displayBuilder _ = "ShowMissingEpisode"

-- | Get all shows scheduled in the next 7 days that are missing episode uploads.
--
-- A show is "missing" if either:
-- - No episode exists for that scheduled time slot
-- - An episode exists but has no audio file uploaded
--
-- Excludes soft-deleted shows. Results are sorted by scheduled date ascending.
getShowsMissingEpisodes :: Hasql.Statement () [ShowMissingEpisode]
getShowsMissingEpisodes =
  interp
    False
    [sql|
    WITH RECURSIVE date_series AS (
      SELECT CURRENT_DATE as date, 1 as n
      UNION ALL
      SELECT date + 1, n + 1
      FROM date_series
      WHERE n < 7
    ),
    schedule_instances AS (
      SELECT DISTINCT
        st.show_id,
        s.title as show_title,
        s.slug as show_slug,
        ds.date as show_date,
        st.day_of_week,
        st.start_time,
        st.end_time,
        st.timezone
      FROM schedule_templates st
      JOIN schedule_template_validity stv ON stv.template_id = st.id
      JOIN shows s ON s.id = st.show_id
      CROSS JOIN date_series ds
      WHERE s.status = 'active'
        AND s.deleted_at IS NULL
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
    )
    SELECT
      si.show_id,
      si.show_title,
      si.show_slug,
      COALESCE(
        STRING_AGG(COALESCE(um.display_name, um.full_name), ', ' ORDER BY sh.joined_at),
        'TBD'
      ) as host_names,
      si.show_date,
      si.day_of_week,
      si.start_time,
      si.end_time
    FROM schedule_instances si
    LEFT JOIN episodes e ON e.show_id = si.show_id
      AND e.scheduled_at = (si.show_date::TEXT || ' ' || si.start_time::TEXT)::TIMESTAMP AT TIME ZONE si.timezone
      AND e.deleted_at IS NULL
    LEFT JOIN show_hosts sh ON sh.show_id = si.show_id AND sh.left_at IS NULL
    LEFT JOIN users u ON u.id = sh.user_id
    LEFT JOIN user_metadata um ON um.user_id = u.id
    WHERE (e.id IS NULL OR e.audio_file_path IS NULL)
    GROUP BY si.show_id, si.show_title, si.show_slug, si.show_date, si.day_of_week, si.start_time, si.end_time
    ORDER BY si.show_date ASC, si.start_time ASC
  |]
