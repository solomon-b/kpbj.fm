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
    templateBelongsToShow,
    getScheduleTemplatesForShow,
    getActiveScheduleTemplatesForShow,
    getPendingScheduleTemplatesForShow,
    checkTimeSlotConflict,
    insertScheduleTemplate,

    -- * Schedule Template Validity Queries
    getActiveValidityPeriodsForTemplate,
    getValidityPeriodsForTemplate,
    insertValidity,
    endValidity,
    restoreValidity,

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
    getShowsMissingEpisodesInDays,
    HostMissingEpisode (..),
    getHostsMissingEpisodesOnDay,

    -- * Recurrence
    dayOfWeekNumber,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Time (Day, DayOfWeek (..), LocalTime (..), TimeOfDay, UTCTime, addUTCTime, dayOfWeek)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (timeOfDayToTime)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Slug (Slug)
import Domain.Types.Timezone (minutesFromMidnight, utcToPacific)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneColumn (..), OneRow (..), interp, sql)
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
-- An immutable recurrence: a weekday plus the weeks of the month it airs on. A show
-- that airs every week holds all five weeks. Every template recurs, so both columns
-- are NOT NULL and 'Effects.Database.Tables.Episodes.getCurrentlyAiringEpisodes' can
-- test the air date against the recurrence without a special case.
data ScheduleTemplate f = ScheduleTemplate
  { stId :: Column f TemplateId,
    stShowId :: Column f Shows.Id,
    stDayOfWeek :: Column f DayOfWeek,
    stWeeksOfMonth :: Column f [Int64],
    stStartTime :: Column f TimeOfDay,
    stEndTime :: Column f TimeOfDay,
    stTimezone :: Column f Text,
    stCreatedAt :: Column f UTCTime,
    stReplayStartTime :: Column f (Maybe TimeOfDay)
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
            stReplayStartTime = "replay_start_time"
          }
    }

-- | Insert type for creating new schedule templates.
data ScheduleTemplateInsert = ScheduleTemplateInsert
  { stiShowId :: Shows.Id,
    stiDayOfWeek :: DayOfWeek,
    stiWeeksOfMonth :: [Int64],
    stiStartTime :: TimeOfDay,
    stiEndTime :: TimeOfDay,
    stiTimezone :: Text,
    stiReplayStartTime :: Maybe TimeOfDay
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
--
-- @show_id@ repeats the template's own show, because @one_active_slot_per_show@ needs
-- a column to group by and an exclusion constraint cannot reach through a join. A
-- composite foreign key onto @schedule_templates (id, show_id)@ holds the two in
-- agreement, and 'insertValidity' fills it from the template rather than the caller.
data ScheduleTemplateValidity f = ScheduleTemplateValidity
  { stvId :: Column f ValidityId,
    stvTemplateId :: Column f TemplateId,
    stvShowId :: Column f Shows.Id,
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
            stvShowId = "show_id",
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

-- | Whether a schedule template belongs to a given show.
--
-- The episode upload and edit forms carry a raw @template_id@ inside their
-- @scheduled_date@ field, so a crafted POST can name any show's template.
-- 'Effects.Database.Tables.Episodes.getCurrentlyAiringEpisode' joins episodes to
-- templates without comparing show ids, which means an episode pointed at another
-- show's template becomes a real airing candidate in that show's window. Every
-- writer of @episodes.schedule_template_id@ checks ownership first.
templateBelongsToShow :: TemplateId -> Shows.Id -> Hasql.Statement () Bool
templateBelongsToShow templateId showId =
  let query =
        interp
          True
          [sql|
        SELECT EXISTS(
          SELECT 1 FROM schedule_templates
          WHERE id = #{templateId} AND show_id = #{showId}
        )
      |]
   in maybe False getOneColumn <$> query

-- | Get all schedule templates for a show.
getScheduleTemplatesForShow :: Shows.Id -> Hasql.Statement () [ScheduleTemplate Result]
getScheduleTemplatesForShow showId =
  run $
    select $
      orderBy ((stDayOfWeek >$< asc) <> (stStartTime >$< asc)) do
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
    SELECT DISTINCT st.id, st.show_id, st.day_of_week, st.weeks_of_month, st.start_time, st.end_time, st.timezone, st.created_at, st.replay_start_time
    FROM schedule_templates st
    JOIN schedule_template_validity stv ON stv.template_id = st.id
    WHERE st.show_id = #{showId}
      AND stv.effective_from <= CURRENT_DATE
      AND (stv.effective_until IS NULL OR stv.effective_until > CURRENT_DATE)
    ORDER BY st.day_of_week, st.start_time
  |]

-- | Get schedule templates with future (pending) validity periods for a show.
--
-- Returns templates whose validity period has not yet begun (effective_from > CURRENT_DATE)
-- and has not been cancelled (effective_until is open-ended or strictly after effective_from).
-- Used to populate the edit form when a future schedule change has been configured.
getPendingScheduleTemplatesForShow :: Shows.Id -> Hasql.Statement () [ScheduleTemplate Result]
getPendingScheduleTemplatesForShow showId =
  interp
    False
    [sql|
    SELECT DISTINCT st.id, st.show_id, st.day_of_week, st.weeks_of_month, st.start_time, st.end_time, st.timezone, st.created_at, st.replay_start_time
    FROM schedule_templates st
    JOIN schedule_template_validity stv ON stv.template_id = st.id
    WHERE st.show_id = #{showId}
      AND stv.effective_from > CURRENT_DATE
      AND (stv.effective_until IS NULL OR stv.effective_until > stv.effective_from)
    ORDER BY st.day_of_week, st.start_time
  |]

-- | Wrapper for single Text result from conflict check.
newtype ConflictingShowTitle = ConflictingShowTitle {getConflictingShowTitle :: Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

-- | Check if a time slot conflicts with another show's schedule (excluding a specific show).
--
-- Returns the title of the conflicting show if there's a conflict, Nothing otherwise.
-- Uses raw SQL because of complex overlap logic and array operations.
-- Excludes soft-deleted shows.
--
-- === Validity windows
--
-- The caller passes the date the proposed slot takes effect as @fromDate@. The
-- proposed slot is treated as the open-ended window @[fromDate, infinity)@, so
-- two validity-window predicates decide which of the other shows' rows can
-- collide with it:
--
--   * @effective_until IS NULL OR effective_until > fromDate@ is the half-open
--     overlap test against the other show's @[effective_from, effective_until)@.
--     A window that already closed on or before @fromDate@ cannot collide, and a
--     window that has not started yet still can, which is how a pending (future)
--     booking gets caught.
--   * @effective_until IS NULL OR effective_until > effective_from@ drops empty
--     windows. A cancelled pending schedule is stored as @effective_until =
--     effective_from@ and never airs, so it must not count as a conflict.
--
-- === The time model
--
-- Every window becomes a half-open range of minutes from midnight of the day it
-- starts on. A window that crosses midnight gets an end above 1440. A window
-- that stops at midnight gets an end of exactly 1440 and does not cross. So a
-- range @[s, e)@ takes @[s, LEAST(e, 1440))@ on its own day, and @[0, e - 1440)@
-- on the next day when @e@ is above 1440.
--
-- Each template gives one window for its primary air time. It gives a second
-- window for its replay, which runs for the same number of minutes.
--
-- === Which days can collide
--
-- The proposed slot recurs on one day of the week. Three comparisons can find a
-- collision, and the query makes all three:
--
--   * The window sits on the same day, and the two day-of-week parts overlap.
--   * The window sits on the next day, and the proposed slot crosses midnight
--     onto it.
--   * The window sits on the previous day, and it crosses midnight onto the
--     proposed slot.
--
-- A recurring template also has to match on @weeks_of_month@. The week can
-- change across midnight, because a date in week @w@ is followed by a date in
-- week @w@ or week @w + 1@. The day after the last day of a month is in week 1,
-- and a month can end in week 4 (February) or week 5. The two cross-midnight
-- comparisons therefore widen the week test to @{w, w + 1}@, plus week 1 when
-- @w@ is 4 or 5. This can report a conflict that a concrete calendar would not
-- produce. It never misses one.
checkTimeSlotConflict ::
  Shows.Id ->
  DayOfWeek ->
  [Int64] ->
  TimeOfDay ->
  TimeOfDay ->
  Day ->
  Hasql.Statement () (Maybe Text)
checkTimeSlotConflict excludeShowId dow weeks start end fromDate =
  let dayNum = dayOfWeekNumber dow
      prevDay = (dayNum + 6) `mod` 7
      nextDay = (dayNum + 1) `mod` 7
      startMin = fromIntegral (minutesFromMidnight start) :: Int64
      rawEndMin = fromIntegral (minutesFromMidnight end) :: Int64
      endMin = if rawEndMin > startMin then rawEndMin else rawEndMin + 1440
   in fmap getConflictingShowTitle
        <$> interp
          False
          [sql|
    WITH templates AS (
      SELECT
        s.title,
        day_of_week_num(st.day_of_week) AS day_num,
        st.weeks_of_month AS weeks,
        (EXTRACT(HOUR FROM st.start_time) * 60 + EXTRACT(MINUTE FROM st.start_time))::INT AS start_min,
        (EXTRACT(HOUR FROM st.end_time) * 60 + EXTRACT(MINUTE FROM st.end_time))::INT
          + CASE WHEN st.end_time > st.start_time THEN 0 ELSE 1440 END AS end_min,
        (EXTRACT(HOUR FROM st.replay_start_time) * 60 + EXTRACT(MINUTE FROM st.replay_start_time))::INT AS replay_start_min
      FROM schedule_templates st
      JOIN schedule_template_validity stv ON stv.template_id = st.id
      JOIN shows s ON s.id = st.show_id
      WHERE s.status = 'active'
        AND s.deleted_at IS NULL
        AND st.show_id != #{excludeShowId}
        AND (stv.effective_until IS NULL OR stv.effective_until > stv.effective_from)
        AND (stv.effective_until IS NULL OR stv.effective_until > #{fromDate})
    ),
    windows AS (
      SELECT title, day_num, weeks, start_min, end_min
      FROM templates
      UNION ALL
      SELECT title, day_num, weeks,
             replay_start_min, replay_start_min + (end_min - start_min)
      FROM templates
      WHERE replay_start_min IS NOT NULL
    ),
    placed AS (
      SELECT
        w.*,
        -- The window runs on a day the proposed slot also runs on.
        w.day_num = #{dayNum} AND w.weeks && #{weeks} AS same_day,
        -- The window runs on the day after a day the proposed slot runs on.
        w.day_num = #{nextDay}
          AND EXISTS (
            SELECT 1 FROM unnest(w.weeks) tw
            WHERE tw = ANY(#{weeks})
               OR tw - 1 = ANY(#{weeks})
               OR (tw = 1 AND (4 = ANY(#{weeks}) OR 5 = ANY(#{weeks})))
          ) AS day_after,
        -- The window runs on the day before a day the proposed slot runs on.
        w.day_num = #{prevDay}
          AND EXISTS (
            SELECT 1 FROM unnest(w.weeks) tw
            WHERE tw = ANY(#{weeks})
               OR tw + 1 = ANY(#{weeks})
               OR (tw >= 4 AND 1 = ANY(#{weeks}))
          ) AS day_before
      FROM windows w
    )
    SELECT p.title
    FROM placed p
    WHERE
      -- Same day: the two day-of-week parts overlap.
      (
        p.same_day
        AND p.start_min < LEAST(#{endMin}, 1440)
        AND LEAST(p.end_min, 1440) > #{startMin}
      )
      OR
      -- The proposed slot crosses midnight and its tail lands on this window.
      -- The tail is [0, endMin - 1440), so it hits any window that starts before
      -- the tail ends. Every window ends after minute 0, so that half is given.
      (
        p.day_after
        AND #{endMin} > 1440
        AND p.start_min < #{endMin} - 1440
      )
      OR
      -- This window crosses midnight and its tail lands on the proposed slot.
      (
        p.day_before
        AND p.end_min > 1440
        AND #{startMin} < p.end_min - 1440
      )
    LIMIT 1
  |]

-- | The day of the week as the integer @EXTRACT(DOW)@ returns. Sunday is 0.
--
-- 'Data.Time.DayOfWeek' numbers Monday 1 through Sunday 7, so only Sunday moves.
dayOfWeekNumber :: DayOfWeek -> Int64
dayOfWeekNumber d = fromIntegral (fromEnum d `mod` 7)

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
    INSERT INTO schedule_templates(show_id, day_of_week, weeks_of_month, start_time, end_time, timezone, created_at, replay_start_time)
    VALUES (#{stiShowId}, #{stiDayOfWeek}::day_of_week, #{stiWeeksOfMonth}, #{stiStartTime}, #{stiEndTime}, #{stiTimezone}, NOW(), #{stiReplayStartTime})
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
    SELECT id, template_id, show_id, effective_from, effective_until
    FROM schedule_template_validity
    WHERE template_id = #{templateId}
      AND effective_from <= CURRENT_DATE
      AND (effective_until IS NULL OR effective_until > CURRENT_DATE)
    ORDER BY effective_from DESC
  |]

-- | Get all validity periods for a template (no date filtering).
--
-- Used to find the effective_from date for pending (future) templates.
getValidityPeriodsForTemplate :: TemplateId -> Hasql.Statement () [ScheduleTemplateValidity Result]
getValidityPeriodsForTemplate templateId =
  interp
    False
    [sql|
    SELECT id, template_id, show_id, effective_from, effective_until
    FROM schedule_template_validity
    WHERE template_id = #{templateId}
    ORDER BY effective_from DESC
  |]

-- | Insert a new validity period.
--
-- Returns the generated ID, or 'Nothing' when no template carries @viTemplateId@.
--
-- @show_id@ is read from the template rather than supplied by the caller, so the two
-- cannot disagree and no caller repeats something the database already knows.
insertValidity :: ValidityInsert -> Hasql.Statement () (Maybe ValidityId)
insertValidity ValidityInsert {..} =
  fmap listToMaybe $
    interp
      False
      [sql|
    INSERT INTO schedule_template_validity (template_id, show_id, effective_from, effective_until)
    SELECT st.id, st.show_id, #{viEffectiveFrom}, #{viEffectiveUntil}
    FROM schedule_templates st
    WHERE st.id = #{viTemplateId}
    RETURNING id
  |]

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

-- | Restore a validity period to open-ended by clearing effective_until.
--
-- Used when cancelling a pending schedule to undo the end-dating of the
-- currently-active validity period.
restoreValidity :: ValidityId -> Hasql.Statement () (Maybe ValidityId)
restoreValidity validityId =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = scheduleTemplateValiditySchema,
            from = pure (),
            set = \_ validity ->
              validity
                { stvEffectiveUntil = lit Nothing
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
-- For shows with replay_start_time set, returns two rows (primary and replay).
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
      AND recurrence_airs_on(day_of_week_num(st.day_of_week), st.weeks_of_month, #{targetDate}::date)
    GROUP BY st.id, st.day_of_week, st.start_time, st.end_time, s.slug, s.title, s.logo_url

    UNION ALL

    -- Replay airings for shows with replay_start_time set
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
      st.replay_start_time as start_time,
      (st.replay_start_time + (
        CASE WHEN st.end_time > st.start_time
          THEN st.end_time - st.start_time
          ELSE (INTERVAL '24 hours' - (st.start_time - st.end_time))
        END
      ))::TIME as end_time,
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
      AND st.replay_start_time IS NOT NULL
      AND stv.effective_from <= #{targetDate}::date
      AND (stv.effective_until IS NULL OR stv.effective_until > #{targetDate}::date)
      AND recurrence_airs_on(day_of_week_num(st.day_of_week), st.weeks_of_month, #{targetDate}::date)
    GROUP BY st.id, st.day_of_week, st.start_time, st.end_time, st.replay_start_time, s.slug, s.title, s.logo_url

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
        AND stv.effective_from <= ds.date
        AND (stv.effective_until IS NULL OR stv.effective_until > ds.date)
        AND recurrence_airs_on(day_of_week_num(st.day_of_week), st.weeks_of_month, ds.date)
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
        (CURRENT_TIMESTAMP AT TIME ZONE 'America/Los_Angeles')::DATE as date,
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
        AND stv.effective_from <= ds.date
        AND (stv.effective_until IS NULL OR stv.effective_until > ds.date)
        AND recurrence_airs_on(day_of_week_num(st.day_of_week), st.weeks_of_month, ds.date)
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
        AND si.show_date >= (CURRENT_TIMESTAMP AT TIME ZONE 'America/Los_Angeles')::DATE
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
          usdDayOfWeek = template.stDayOfWeek,
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


-- | A single host of a show scheduled on a specific day that is missing an episode upload.
--
-- Unlike 'ShowMissingEpisode' which aggregates host names, this returns one row per host
-- with their email address, suitable for sending individual notification emails.
data HostMissingEpisode = HostMissingEpisode
  { hmeHostEmail :: Text,
    hmeHostDisplayName :: Text,
    hmeShowTitle :: Text,
    hmeShowSlug :: Slug,
    hmeShowDate :: Day,
    hmeDayOfWeek :: DayOfWeek,
    hmeStartTime :: TimeOfDay,
    hmeEndTime :: TimeOfDay
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (DecodeRow)

instance Display HostMissingEpisode where
  displayBuilder _ = "HostMissingEpisode"

-- | Get all shows scheduled in the next 7 days that are missing episode uploads.
--
-- A show is "missing" if either:
-- - No episode exists for that scheduled time slot
-- - An episode exists but has no audio file uploaded
--
-- Excludes soft-deleted shows. Results are sorted by scheduled date ascending.
getShowsMissingEpisodes :: Hasql.Statement () [ShowMissingEpisode]
getShowsMissingEpisodes = getShowsMissingEpisodesInDays 7

-- | Get all shows scheduled in the next N days that are missing episode uploads.
--
-- A show is "missing" if either:
-- - No episode exists for that scheduled time slot
-- - An episode exists but has no audio file uploaded
--
-- Excludes soft-deleted shows. Results are sorted by scheduled date ascending.
getShowsMissingEpisodesInDays :: Int32 -> Hasql.Statement () [ShowMissingEpisode]
getShowsMissingEpisodesInDays days =
  interp
    False
    [sql|
    WITH RECURSIVE date_series AS (
      SELECT (CURRENT_TIMESTAMP AT TIME ZONE 'America/Los_Angeles')::DATE as date, 1 as n
      UNION ALL
      SELECT date + 1, n + 1
      FROM date_series
      WHERE n < #{days}
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
        AND stv.effective_from <= ds.date
        AND (stv.effective_until IS NULL OR stv.effective_until > ds.date)
        AND recurrence_airs_on(day_of_week_num(st.day_of_week), st.weeks_of_month, ds.date)
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


-- | Get hosts of shows missing episodes on exactly N days from now.
--
-- Returns one row per host per missing show. Used by the episode-check job
-- to send individual reminder emails. Only includes shows with at least one
-- active host assigned.
getHostsMissingEpisodesOnDay :: Int32 -> Hasql.Statement () [HostMissingEpisode]
getHostsMissingEpisodesOnDay days =
  interp
    False
    [sql|
    WITH target_date AS (
      SELECT (CURRENT_TIMESTAMP AT TIME ZONE 'America/Los_Angeles')::DATE + #{days} AS date
    ),
    schedule_instances AS (
      SELECT DISTINCT
        st.show_id,
        s.title as show_title,
        s.slug as show_slug,
        td.date as show_date,
        st.day_of_week,
        st.start_time,
        st.end_time,
        st.timezone
      FROM schedule_templates st
      JOIN schedule_template_validity stv ON stv.template_id = st.id
      JOIN shows s ON s.id = st.show_id
      CROSS JOIN target_date td
      WHERE s.status = 'active'
        AND s.deleted_at IS NULL
        AND stv.effective_from <= td.date
        AND (stv.effective_until IS NULL OR stv.effective_until > td.date)
        AND recurrence_airs_on(day_of_week_num(st.day_of_week), st.weeks_of_month, td.date)
    )
    SELECT
      u.email,
      COALESCE(um.display_name, um.full_name, u.email) as host_display_name,
      si.show_title,
      si.show_slug,
      si.show_date,
      si.day_of_week,
      si.start_time,
      si.end_time
    FROM schedule_instances si
    LEFT JOIN episodes e ON e.show_id = si.show_id
      AND e.scheduled_at = (si.show_date::TEXT || ' ' || si.start_time::TEXT)::TIMESTAMP AT TIME ZONE si.timezone
      AND e.deleted_at IS NULL
    JOIN show_hosts sh ON sh.show_id = si.show_id AND sh.left_at IS NULL
    JOIN users u ON u.id = sh.user_id AND u.deleted_at IS NULL
    JOIN user_metadata um ON um.user_id = u.id
    WHERE (e.id IS NULL OR e.audio_file_path IS NULL)
    ORDER BY si.show_date ASC, si.start_time ASC, u.email ASC
  |]
