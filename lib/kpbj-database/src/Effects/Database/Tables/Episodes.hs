{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @episodes@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
module Effects.Database.Tables.Episodes
  ( -- * Id Type
    Id (..),

    -- * Episode Number Type
    EpisodeNumber (..),

    -- * Table Definition
    Episode (..),
    episodeSchema,

    -- * Model (Result alias)
    Model,
    isUnaired,
    isAired,

    -- * Insert Type
    Insert (..),

    -- * Update Types
    Update (..),
    FileUpdate (..),
    ScheduleSlotUpdate (..),

    -- * Archived Filter
    ArchivedFilter (..),

    -- * Queries
    getPublishedEpisodesForShow,
    getPublishedEpisodesWithShows,
    getEpisodesForShow,
    getEpisodeByShowAndNumber,
    getEpisodeById,
    getEpisodeByAudioPath,
    getEpisodesByUser,
    getCurrentlyAiringEpisode,
    getCurrentlyAiringEpisodes,
    insertEpisode,
    updateEpisode,
    updateEpisodeFiles,
    updateScheduledSlot,
    clearScheduledSlot,
    deleteEpisode,
    restoreEpisode,
    getLiveEpisodeAtAirTime,
    clearTemplateForUpcomingEpisodes,
    migrateUpcomingEpisodes,
    closeSchedulesAndDetachEpisodes,
    getUpcomingEpisodesForTemplates,

    -- * Result Types
    EpisodeWithShow (..),
    SearchResult (..),
    UpcomingEpisodeRef (..),

    -- * Search Queries
    searchEpisodesWithAudio,

    -- * Tag Junction Queries
    getTagsForEpisode,
    replaceEpisodeTags,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..), RecordInstance (..))
import Data.Time (Day, UTCTime)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EpisodeTags qualified as EpisodeTags
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Enum, Insert, Update)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for episode primary keys.
--
-- Provides type safety to prevent mixing up IDs from different tables.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq, DBOrd)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Episode Number Type

-- | Episode number within a show, auto-assigned by PostgreSQL trigger.
newtype EpisodeNumber = EpisodeNumber {unEpisodeNumber :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq, DBOrd)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Table Definition

-- | The @episodes@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data Episode f = Episode
  { id :: Column f Id,
    showId :: Column f Shows.Id,
    description :: Column f (Maybe Text),
    episodeNumber :: Column f EpisodeNumber,
    audioFilePath :: Column f (Maybe Text),
    audioFileSize :: Column f (Maybe Int64),
    audioMimeType :: Column f (Maybe Text),
    durationSeconds :: Column f (Maybe Int64),
    artworkUrl :: Column f (Maybe Text),
    scheduleTemplateId :: Column f (Maybe ShowSchedule.TemplateId),
    scheduledAt :: Column f (Maybe UTCTime),
    publishedAt :: Column f (Maybe UTCTime),
    deletedAt :: Column f (Maybe UTCTime),
    createdBy :: Column f User.Id,
    createdAt :: Column f UTCTime,
    updatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (Episode f)

deriving stock instance (f ~ Result) => Eq (Episode f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (Episode Result)

-- | Display instance for Episode Result.
instance Display (Episode Result) where
  displayBuilder ep =
    "Episode { id = "
      <> displayBuilder ep.id
      <> ", episodeNumber = "
      <> displayBuilder ep.episodeNumber
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @Episode Result@.
type Model = Episode Result

-- | An episode is unaired if it has no scheduled date or its date is in the future.
isUnaired :: UTCTime -> Model -> Bool
isUnaired currentTime episode = case episode.scheduledAt of
  Nothing -> True
  Just sa -> sa > currentTime

-- | An episode has aired if it has a scheduled date that has passed.
isAired :: UTCTime -> Model -> Bool
isAired currentTime = not . isUnaired currentTime

-- | Table schema connecting the Haskell type to the database table.
episodeSchema :: TableSchema (Episode Name)
episodeSchema =
  TableSchema
    { name = "episodes",
      columns =
        Episode
          { id = "id",
            showId = "show_id",
            description = "description",
            episodeNumber = "episode_number",
            audioFilePath = "audio_file_path",
            audioFileSize = "audio_file_size",
            audioMimeType = "audio_mime_type",
            durationSeconds = "duration_seconds",
            artworkUrl = "artwork_url",
            scheduleTemplateId = "schedule_template_id",
            scheduledAt = "scheduled_at",
            publishedAt = "published_at",
            deletedAt = "deleted_at",
            createdBy = "created_by",
            createdAt = "created_at",
            updatedAt = "updated_at"
          }
    }

--------------------------------------------------------------------------------
-- Junction Table (episode_tag_assignments)

-- | The @episode_tag_assignments@ junction table definition (internal, not exported).
data EpisodeTagAssignment f = EpisodeTagAssignment
  { etaEpisodeId :: Column f Id,
    etaTagId :: Column f EpisodeTags.Id
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- | Schema for the @episode_tag_assignments@ junction table.
episodeTagAssignmentSchema :: TableSchema (EpisodeTagAssignment Name)
episodeTagAssignmentSchema =
  TableSchema
    { name = "episode_tag_assignments",
      columns =
        EpisodeTagAssignment
          { etaEpisodeId = "episode_id",
            etaTagId = "tag_id"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new episodes.
--
-- Episodes are always created as published (published_at = NOW()).
data Insert = Insert
  { eiId :: Shows.Id,
    eiDescription :: Maybe Text,
    eiAudioFilePath :: Maybe Text,
    eiAudioFileSize :: Maybe Int64,
    eiAudioMimeType :: Maybe Text,
    eiDurationSeconds :: Maybe Int64,
    eiArtworkUrl :: Maybe Text,
    eiScheduleTemplateId :: Maybe ShowSchedule.TemplateId,
    eiScheduledAt :: Maybe UTCTime,
    eiCreatedBy :: User.Id
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Update Types

-- | Episode Update data for partial updates.
data Update = Update
  { euId :: Id,
    euDescription :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Update)

-- | Episode file update data for updating audio and artwork files.
--
-- The clear flags allow explicitly setting fields to NULL (removing the file).
-- When a clear flag is True, the corresponding field is set to NULL regardless
-- of the Maybe value. When False, Nothing preserves existing and Just sets new.
data FileUpdate = FileUpdate
  { efuId :: Id,
    efuAudioFilePath :: Maybe Text,
    efuArtworkUrl :: Maybe Text,
    efuDurationSeconds :: Maybe Int64, -- Duration when new audio is uploaded
    efuClearAudio :: Bool, -- If True, set audio_file_path to NULL
    efuClearArtwork :: Bool -- If True, set artwork_url to NULL
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance FileUpdate)

-- | Episode schedule slot update for changing the scheduled time slot.
data ScheduleSlotUpdate = ScheduleSlotUpdate
  { essuId :: Id,
    essuScheduleTemplateId :: ShowSchedule.TemplateId,
    essuScheduledAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance ScheduleSlotUpdate)

--------------------------------------------------------------------------------
-- Result Types

-- | Data type for episode archive results with show information.
--
-- This flattens Model fields + show info for easier decoding.
data EpisodeWithShow = EpisodeWithShow
  { ewsId :: Id,
    ewsShowId :: Shows.Id,
    ewsDescription :: Maybe Text,
    ewsEpisodeNumber :: EpisodeNumber,
    ewsAudioFilePath :: Maybe Text,
    ewsAudioFileSize :: Maybe Int64,
    ewsAudioMimeType :: Maybe Text,
    ewsDurationSeconds :: Maybe Int64,
    ewsArtworkUrl :: Maybe Text,
    ewsScheduleTemplateId :: Maybe ShowSchedule.TemplateId,
    ewsScheduledAt :: Maybe UTCTime,
    ewsPublishedAt :: Maybe UTCTime,
    ewsDeletedAt :: Maybe UTCTime,
    ewsCreatedBy :: User.Id,
    ewsCreatedAt :: UTCTime,
    ewsUpdatedAt :: UTCTime,
    ewsShowTitle :: Text,
    ewsShowSlug :: Slug,
    ewsHostDisplayName :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance EpisodeWithShow)

-- | Search result for force-play episode lookup.
data SearchResult = SearchResult
  { srId :: Id,
    srShowTitle :: Text,
    srEpisodeNumber :: EpisodeNumber,
    srScheduledAt :: Maybe UTCTime,
    srDurationSeconds :: Maybe Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance SearchResult)

-- | A reference to an upcoming episode still attached to a template.
--
-- @scheduled_at@ is non-null and @episode_number@ is non-null by the query's
-- WHERE clause and the @episodes@ schema, so both are unwrapped here.
data UpcomingEpisodeRef = UpcomingEpisodeRef
  { uerId :: Id,
    uerEpisodeNumber :: EpisodeNumber,
    uerScheduledAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance UpcomingEpisodeRef)

--------------------------------------------------------------------------------
-- Queries

-- | Get published episodes for a show (not deleted, scheduled in the past).
getPublishedEpisodesForShow :: UTCTime -> Shows.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getPublishedEpisodesForShow currentTime showId' (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy ((.publishedAt) >$< nullsLast desc) do
            ep <- each episodeSchema
            where_ $ ep.showId ==. lit showId'
            where_ $ isNull ep.deletedAt
            where_ $ isNonNull ep.scheduledAt
            where_ $ ep.scheduledAt <=. nullify (lit currentTime)
            pure ep

-- | Get published episodes across all non-deleted shows, each paired with its
-- show, ordered by publish date (newest first).
--
-- Powers the public @/archive@ page. Includes episodes from inactive shows —
-- only soft-deleted shows are excluded. "Published" matches
-- 'getPublishedEpisodesForShow': not deleted and scheduled in the past.
getPublishedEpisodesWithShows :: UTCTime -> Limit -> Offset -> Hasql.Statement () [(Model, Shows.Model)]
getPublishedEpisodesWithShows currentTime (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy ((\(ep, _s) -> ep.publishedAt) >$< nullsLast desc) do
            ep <- each episodeSchema
            s <- each Shows.showSchema
            where_ $ ep.showId ==. s.id
            where_ $ isNull ep.deletedAt
            where_ $ isNonNull ep.scheduledAt
            where_ $ ep.scheduledAt <=. nullify (lit currentTime)
            where_ $ isNull s.deletedAt
            pure (ep, s)

-- | Whether a read includes the episodes that staff archived.
--
-- Archive is the station's moderation tool, so 'ExcludeArchived' is the answer
-- for anything a listener can reach. Only the staff-facing dashboard reads
-- 'IncludeArchived', and it decides from the caller's role.
--
-- This is a parameter rather than a second function so that GHC makes every
-- call site say which it wants. A query that quietly omitted the predicate is
-- what let an archived episode stay on the public site.
data ArchivedFilter
  = ExcludeArchived
  | IncludeArchived
  deriving stock (Show, Eq)

-- | Restrict a query to the live rows, unless the caller asked for all of them.
applyArchivedFilter :: ArchivedFilter -> Episode Expr -> Query ()
applyArchivedFilter = \case
  ExcludeArchived -> where_ . isNull . (.deletedAt)
  IncludeArchived -> const (pure ())

-- | Get episodes for a show (for hosts viewing their own show).
--
-- Ordered by scheduled date descending.
getEpisodesForShow :: Shows.Id -> ArchivedFilter -> Limit -> Offset -> Hasql.Statement () [Model]
getEpisodesForShow showId' archived (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy ((.scheduledAt) >$< nullsLast desc) do
            ep <- each episodeSchema
            where_ $ ep.showId ==. lit showId'
            applyArchivedFilter archived ep
            pure ep

-- | Get an episode by show slug and episode number.
--
-- Joins with shows table to filter by show slug.
--
-- Pass 'ExcludeArchived' for anything a listener reaches. This query backs the
-- public episode page, so without that predicate the page kept serving an
-- archived episode and its audio at a guessable URL.
getEpisodeByShowAndNumber :: Slug -> EpisodeNumber -> ArchivedFilter -> Hasql.Statement () (Maybe Model)
getEpisodeByShowAndNumber showSlug episodeNum archived = fmap listToMaybe $ run $ select do
  ep <- each episodeSchema
  s <- each Shows.showSchema
  where_ $ showId ep ==. Shows.id s
  where_ $ Shows.slug s ==. lit showSlug
  where_ $ episodeNumber ep ==. lit episodeNum
  applyArchivedFilter archived ep
  pure ep

-- | Get non-deleted episode by ID.
getEpisodeById :: Id -> Hasql.Statement () (Maybe Model)
getEpisodeById episodeId = fmap listToMaybe $ run $ select do
  ep <- each episodeSchema
  where_ $ ep.id ==. lit episodeId
  where_ $ isNull (ep.deletedAt)
  pure ep

-- | Find an episode by its audio file path (object key).
--
-- Resolves a media URL back to its episode record by matching the stored
-- @audio_file_path@ column.
--
-- This matches a soft-deleted episode as well. The only caller logs a play to
-- @playback_history@ after the audio went out over the air. A play belongs to
-- the episode whose file played, and a later delete does not undo that. The
-- airing queries filter deleted rows, so a deleted episode reaches this
-- function only when someone deletes it while it is playing.
getEpisodeByAudioPath :: Text -> Hasql.Statement () (Maybe Model)
getEpisodeByAudioPath audioPath = fmap listToMaybe $ run $ select do
  ep <- each episodeSchema
  where_ $ ep.audioFilePath ==. nullify (lit audioPath)
  pure ep

-- | Get non-deleted episodes by user (episodes they created).
getEpisodesByUser :: User.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getEpisodesByUser userId (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy ((.createdAt) >$< desc) do
            ep <- each episodeSchema
            where_ $ ep.createdBy ==. lit userId
            where_ $ isNull ep.deletedAt
            pure ep

-- | Every row that the schedule says should be on air at the given time.
--
-- Liquidsoap polls @\/api\/playout\/now@, which calls this and broadcasts the
-- first row.
--
-- == What a row is
--
-- The query works in (episode, window) pairs, not in episodes. A template with a
-- @replay_start_time@ gives its episode two windows, so one episode can produce
-- two rows. See "Why there can be more than one row" below.
--
-- == The airing window
--
-- @schedule_templates@ holds a @day_of_week@, a @weeks_of_month@, a
-- @start_time@, an @end_time@, an optional @replay_start_time@, and a
-- @timezone@. A separate table, @schedule_template_validity@, has one or more
-- rows per template, each with an @effective_from@ and an @effective_until@.
-- Those two columns bound the air dates the template applies to.
--
-- An overnight slot has nowhere to record that its end falls on the following
-- date, so the schema encodes that in the order of the two @time@ values. A
-- Monday 23:00 to 02:00 show is one row with @start_time = '23:00'@ and
-- @end_time = '02:00'@.
--
-- A window therefore opens at @start_time@ on the episode's air date and closes
-- at @end_time@, on the following date when @end_time <= start_time@. Two equal
-- values give a 24-hour window. The air date is
-- @(scheduled_at AT TIME ZONE 'America\/Los_Angeles')::DATE@.
--
-- A replay window opens at @replay_start_time@ and runs for the same length as
-- the primary, and it wraps the same way.
--
-- The zone is the literal @'America\/Los_Angeles'@ in every place it appears
-- here. @schedule_templates.timezone@ is not read.
--
-- == Why the window is a pair of timestamptz values
--
-- A @time@ carries no date and no zone, and the Pacific offset changes twice a
-- year, so the two are not interchangeable:
--
-- * On 2025-11-02 the clock runs 01:00 to 02:00 twice. The @time@ @01:30@
--   matches both 08:30 UTC and 09:30 UTC, and a @00:00@ to @02:00@ window covers
--   3 elapsed hours.
-- * On 2026-03-08 the clock jumps from 01:59 to 03:00. The @time@ @02:30@
--   matches no @timestamptz@, and a @02:00@ to @04:00@ window covers 1 elapsed
--   hour.
--
-- === A clock time inside the repeated hour
--
-- On the fall-back date a @time@ from 01:00 to 01:59 names two instants, an hour
-- apart. @AT TIME ZONE@ alone returns the later one. Both endpoints take the
-- earlier one instead, through
-- @LEAST(t AT TIME ZONE z, (t - INTERVAL '1 hour') AT TIME ZONE z + INTERVAL '1 hour')@.
--
-- So a 01:00 to 02:00 slot opens when the clock first reads 01:00 and covers the
-- 2 elapsed hours it really holds. A 1-hour episode plays the first of them and
-- leaves the second silent. The silence belongs at the end of a slot, where a
-- short episode always puts it, and not at the start where nothing explains it.
--
-- The correction moves the instant back by one hour, the size of the Pacific
-- shift. Every other @time@ of the year names one instant, so @LEAST@ picks that
-- instant from both arms and the correction changes nothing. That includes the
-- spring-forward gap.
--
-- === A slot that the spring-forward change deletes
--
-- On 2026-03-08 no instant reads as a Pacific time from 02:00 to 02:59, and
-- PostgreSQL normalizes such a time forward by an hour. A slot that opens in
-- that range therefore has no elapsed time to run in:
--
-- * 02:00 to 03:00 gives @window_start = window_end@, an empty window
-- * 02:30 to 03:00 gives @window_start@ /after/ @window_end@, an inverted window
--
-- Neither airs. @window_stop@ is at most @window_end@, and a row airs only when
-- the current time is at or after @window_start@ and before @window_stop@, so
-- both shapes fail the test for every @duration_seconds@. The hour is dead air
-- and nothing here reports it. The old query behaved the same way, for the same
-- reason: that clock reading never occurs.
--
-- == When a row airs
--
-- @
-- window_stop = LEAST(window_end, window_start + duration_seconds)
-- airing      = currentTime >= window_start AND currentTime < window_stop
-- @
--
-- @LEAST@ skips a NULL, so an episode with no @duration_seconds@ runs to
-- @window_end@. @duration_seconds@ is written by browser JavaScript at upload
-- from @HTMLAudioElement.duration@, in @Component.AudioDurationScript@, and
-- nothing validates it. A value of 0 makes @window_stop@ equal @window_start@,
-- so that episode never airs.
--
-- @
-- Slot: 2 PM ─────────────────────────── 4 PM
--       │                                │
--       ├── duration=30min ──┤           │
--       │                    │           │
--       ▼                    ▼           ▼
--      2:00 PM            2:30 PM     4:00 PM
--       │◀── AIRING ──────▶│◀── NOT ──▶│
-- @
--
-- == What the query excludes
--
-- * an episode with no @audio_file_path@, or with @deleted_at@ set
-- * an episode of a show that is not @active@, or that has @deleted_at@ set
-- * an episode with a NULL @schedule_template_id@, dropped by the join, or a
--   NULL @scheduled_at@, dropped because the air date is then NULL
-- * an air date outside @[effective_from, effective_until)@ on the joined
--   validity row
-- * an air date the recurrence does not cover, by @recurrence_airs_on@ over
--   @day_of_week@ and @weeks_of_month@
-- * the replay row of a template with no @replay_start_time@
-- * an air date that is neither today nor yesterday in Pacific. This prunes the
--   scan. The window comparison decides the answer
--
-- It does __not__ exclude an episode with a NULL @published_at@.
--
-- == Why there can be more than one row
--
-- Rows are ordered @is_replay, scheduled_at DESC, id DESC@, so the first row
-- does not change between polls while the data is unchanged. There is no
-- @LIMIT@. A second row means one of:
--
-- * two shows hold overlapping slots
-- * one show holds two overlapping slots
-- * one episode matches both its primary and its replay window. That happens
--   whenever @replay_start_time@ falls inside the primary window, and no
--   constraint forbids it
-- * one template has two @schedule_template_validity@ rows covering the air
--   date. The join multiplies and there is no @DISTINCT@, so the two rows are
--   identical. That table is constrained only by
--   @UNIQUE (template_id, effective_from)@
--
-- The first two are scheduling conflicts. The last two are one airing counted
-- twice. The query does not distinguish them.
getCurrentlyAiringEpisodes :: UTCTime -> Hasql.Statement () [Model]
getCurrentlyAiringEpisodes currentTime =
  interp
    False
    [sql|
    WITH airing_windows AS (
      SELECT
        e.id, e.show_id, e.description, e.episode_number, e.audio_file_path,
        e.audio_file_size, e.audio_mime_type, e.duration_seconds, e.artwork_url,
        e.schedule_template_id, e.scheduled_at, e.published_at, e.deleted_at,
        e.created_by, e.created_at, e.updated_at,
        v.is_replay,
        w.window_start,
        -- The episode stops at the end of its slot or at the end of its audio,
        -- whichever comes first. LEAST skips a NULL duration.
        LEAST(
          w.window_end,
          w.window_start + e.duration_seconds * INTERVAL '1 second'
        ) AS window_stop
      FROM episodes e
      JOIN schedule_templates st ON st.id = e.schedule_template_id
      JOIN schedule_template_validity stv ON stv.template_id = st.id
      JOIN shows s ON s.id = e.show_id
      -- The date the episode airs on, in the station's timezone.
      CROSS JOIN LATERAL (
        SELECT (e.scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE AS air_date
      ) d
      -- The length of the slot as a time interval. An overnight slot wraps midnight.
      CROSS JOIN LATERAL (
        SELECT
          CASE WHEN st.end_time > st.start_time
            THEN st.end_time - st.start_time
            ELSE INTERVAL '24 hours' - (st.start_time - st.end_time)
          END AS slot_length
      ) sl
      -- One row for the primary window, one for the replay. A replay runs for
      -- the same length as its primary.
      CROSS JOIN LATERAL (
        VALUES
          (FALSE, st.start_time, st.end_time),
          (TRUE, st.replay_start_time, (st.replay_start_time + sl.slot_length)::TIME)
      ) AS v(is_replay, start_time, end_time)
      -- The window as a pair of local timestamps. A window that closes at or
      -- before it opens runs onto the next date.
      CROSS JOIN LATERAL (
        SELECT
          d.air_date + v.start_time AS local_start,
          (d.air_date + CASE WHEN v.end_time <= v.start_time THEN 1 ELSE 0 END) + v.end_time
            AS local_end
      ) l
      -- The same pair as timestamptz values.
      --
      -- On the fall-back date a local time from 01:00 to 01:59 names two
      -- instants, an hour apart, and AT TIME ZONE alone returns the later one.
      -- LEAST against the same conversion an hour earlier takes the first
      -- instant instead, so a slot opens when the clock first reads its start
      -- time. A short episode then leaves its silence at the end of the slot.
      -- One hour is the size of the Pacific shift, and the correction is a
      -- no-op on every other local time of the year.
      CROSS JOIN LATERAL (
        SELECT
          LEAST(
            l.local_start AT TIME ZONE 'America/Los_Angeles',
            (l.local_start - INTERVAL '1 hour') AT TIME ZONE 'America/Los_Angeles'
              + INTERVAL '1 hour'
          ) AS window_start,
          LEAST(
            l.local_end AT TIME ZONE 'America/Los_Angeles',
            (l.local_end - INTERVAL '1 hour') AT TIME ZONE 'America/Los_Angeles'
              + INTERVAL '1 hour'
          ) AS window_end
      ) w
      WHERE
        -- A template with no replay contributes its primary row only.
        v.start_time IS NOT NULL
        AND e.audio_file_path IS NOT NULL
        AND e.deleted_at IS NULL
        AND s.status = 'active'
        AND s.deleted_at IS NULL
        -- A window opens on the air date and closes at most one date later, so
        -- only these two dates can hold the current time. This prunes the
        -- scan. The comparison below decides the answer.
        AND d.air_date BETWEEN (#{currentTime} AT TIME ZONE 'America/Los_Angeles')::DATE - 1
                           AND (#{currentTime} AT TIME ZONE 'America/Los_Angeles')::DATE
        AND stv.effective_from <= d.air_date
        AND (stv.effective_until IS NULL OR stv.effective_until > d.air_date)
        -- The air date must be a date the template holds.
        AND recurrence_airs_on(day_of_week_num(st.day_of_week), st.weeks_of_month, d.air_date)
    )
    SELECT
      id, show_id, description, episode_number, audio_file_path,
      audio_file_size, audio_mime_type, duration_seconds, artwork_url,
      schedule_template_id, scheduled_at, published_at, deleted_at,
      created_by, created_at, updated_at
    FROM airing_windows
    WHERE #{currentTime} >= window_start
      AND #{currentTime} < window_stop
    -- A primary airing beats a replay. Past that the order only has to be
    -- stable, so the stream does not flip between two claimants on each poll.
    ORDER BY is_replay, scheduled_at DESC, id DESC
  |]

-- | The first row of 'getCurrentlyAiringEpisodes', or Nothing.
--
-- Discards the second row, so the caller cannot tell a single airing from an
-- overlap or a duplicate. Prefer 'getCurrentlyAiringEpisodes' where the caller
-- can act on that.
getCurrentlyAiringEpisode :: UTCTime -> Hasql.Statement () (Maybe Model)
getCurrentlyAiringEpisode = fmap listToMaybe . getCurrentlyAiringEpisodes

-- | Insert a new episode.
--
-- Episode numbers are auto-assigned by a PostgreSQL trigger.
-- Episodes are always created as published (published_at = NOW()).
insertEpisode :: Insert -> Hasql.Statement () (Maybe Id)
insertEpisode Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = episodeSchema,
            rows =
              values
                [ Episode
                    { id = nextId "episodes_id_seq",
                      showId = lit eiId,
                      description = lit eiDescription,
                      episodeNumber = unsafeDefault,
                      audioFilePath = lit eiAudioFilePath,
                      audioFileSize = lit eiAudioFileSize,
                      audioMimeType = lit eiAudioMimeType,
                      durationSeconds = lit eiDurationSeconds,
                      artworkUrl = lit eiArtworkUrl,
                      scheduleTemplateId = lit eiScheduleTemplateId,
                      scheduledAt = lit eiScheduledAt,
                      publishedAt = nullify now,
                      deletedAt = Rel8.null,
                      createdBy = lit eiCreatedBy,
                      createdAt = now,
                      updatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning (.id)
          }

-- | Update an episode with partial data (for editing).
updateEpisode :: Update -> Hasql.Statement () (Maybe Id)
updateEpisode Update {..} =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = episodeSchema,
            from = pure (),
            set = \_ ep ->
              ep
                { description = lit euDescription,
                  updatedAt = now
                },
            updateWhere = \_ ep -> ep.id ==. lit euId,
            returning = Returning (.id)
          }

-- | Update an episode's audio and artwork files.
--
-- For audio: If efuClearAudio is True, sets to NULL. Otherwise,
-- Nothing preserves existing and Just sets new value.
-- For artwork: If efuClearArtwork is True, sets to NULL. Otherwise,
-- Nothing preserves existing and Just sets new value.
-- For duration: Only updated if new audio is uploaded (efuDurationSeconds is Just).
updateEpisodeFiles :: FileUpdate -> Hasql.Statement () (Maybe Id)
updateEpisodeFiles FileUpdate {..} =
  interp
    False
    [sql|
    UPDATE episodes
    SET audio_file_path = CASE
          WHEN #{efuClearAudio} THEN NULL
          ELSE COALESCE(#{efuAudioFilePath}, audio_file_path)
        END,
        artwork_url = CASE
          WHEN #{efuClearArtwork} THEN NULL
          ELSE COALESCE(#{efuArtworkUrl}, artwork_url)
        END,
        duration_seconds = COALESCE(#{efuDurationSeconds}, duration_seconds),
        updated_at = NOW()
    WHERE id = #{efuId}
    RETURNING id
  |]

-- | Update an episode's scheduled time slot.
--
-- Changes both the schedule template reference and the scheduled_at timestamp.
updateScheduledSlot :: ScheduleSlotUpdate -> Hasql.Statement () (Maybe Id)
updateScheduledSlot ScheduleSlotUpdate {..} =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = episodeSchema,
            from = pure (),
            set = \_ ep ->
              ep
                { scheduleTemplateId = nullify (lit essuScheduleTemplateId),
                  scheduledAt = nullify (lit essuScheduledAt),
                  updatedAt = now
                },
            updateWhere = \_ ep -> ep.id ==. lit essuId,
            returning = Returning (.id)
          }

-- | Clear the schedule slot of one episode.
--
-- The two columns change together. A template with no date gives no airing. A
-- date with no template gives no window. Each column alone has no use.
-- 'clearTemplateForUpcomingEpisodes' writes the same pair for a full template.
--
-- 'getCurrentlyAiringEpisodes' joins the template with an inner join. A cleared
-- episode therefore leaves the stream immediately.
-- @unique_episode_scheduled_at@ covers the live rows only, and NULL values never
-- collide. A new episode can take the free air time immediately. The cleared
-- episode keeps its number, its audio, and its tracks.
clearScheduledSlot :: Id -> Hasql.Statement () (Maybe Id)
clearScheduledSlot episodeId =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = episodeSchema,
            from = pure (),
            set = \_ ep ->
              ep
                { scheduleTemplateId = lit Nothing,
                  scheduledAt = lit Nothing,
                  updatedAt = now
                },
            updateWhere = \_ ep -> ep.id ==. lit episodeId,
            returning = Returning (.id)
          }

-- | Delete an episode (soft delete by setting deleted_at timestamp).
--
-- Returns the archived row, so a caller can render it without a second read.
-- 'getEpisodeById' cannot serve that, because it returns the live rows only.
deleteEpisode :: Id -> Hasql.Statement () (Maybe Model)
deleteEpisode episodeId =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = episodeSchema,
            from = pure (),
            set = \_ ep ->
              ep
                { deletedAt = nullify now,
                  updatedAt = now
                },
            updateWhere = \_ ep -> ep.id ==. lit episodeId,
            returning = Returning (\ep -> ep)
          }

-- | Find the live episode of a show that already holds an air time.
--
-- 'restoreEpisode' calls this first. @unique_episode_scheduled_at@ covers the
-- live rows only, so another episode can take the air time while this one sits
-- archived. Without the check the restore fails on the index, and the handler
-- can only report a database error.
--
-- The given episode is excluded, so an episode never collides with itself.
getLiveEpisodeAtAirTime :: Shows.Id -> UTCTime -> Id -> Hasql.Statement () (Maybe Model)
getLiveEpisodeAtAirTime showId' airTime exceptId = fmap listToMaybe $ run $ select do
  ep <- each episodeSchema
  where_ $ ep.showId ==. lit showId'
  where_ $ ep.scheduledAt ==. nullify (lit airTime)
  where_ $ ep.id /=. lit exceptId
  where_ $ isNull ep.deletedAt
  pure ep

-- | Restore an archived episode by clearing @deleted_at@.
--
-- The episode returns to the public site, so only staff and admins may run this.
--
-- This can fail on @unique_episode_scheduled_at@. That index covers the live
-- rows only, so another episode of the show can take the air time while this one
-- sits archived. The caller reports that collision rather than showing a 500.
restoreEpisode :: Id -> Hasql.Statement () (Maybe Model)
restoreEpisode episodeId =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = episodeSchema,
            from = pure (),
            set = \_ ep ->
              ep
                { deletedAt = lit Nothing,
                  updatedAt = now
                },
            updateWhere = \_ ep -> ep.id ==. lit episodeId &&. isNonNull ep.deletedAt,
            returning = Returning (\ep -> ep)
          }

-- | Clear schedule_template_id for upcoming episodes tied to a given template,
-- gated by the change's start date.
--
-- Used when a schedule template is invalidated (e.g., timeslot changed) to
-- explicitly detach future episodes rather than leaving them with a stale FK.
-- Only episodes whose Pacific air date is on or after @fromDate@ are detached, so
-- when a change is deferred to a future date the interim episodes keep airing on
-- the old slot until then. The @scheduled_at > NOW()@ guard still applies, so an
-- episode that already aired earlier today is never detached. Returns the IDs of
-- affected episodes for logging.
clearTemplateForUpcomingEpisodes :: ShowSchedule.TemplateId -> Day -> Hasql.Statement () [Id]
clearTemplateForUpcomingEpisodes templateId fromDate =
  interp
    False
    [sql|
    UPDATE episodes
    SET schedule_template_id = NULL, scheduled_at = NULL, updated_at = NOW()
    WHERE schedule_template_id = #{templateId}
      AND scheduled_at > NOW()
      AND (scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE >= #{fromDate}
      AND deleted_at IS NULL
    RETURNING id
  |]

-- | Move upcoming episodes from one template to another. Keep their air times.
--
-- A deferred replay change writes a second template that carries the new replay
-- time from the change date. The two templates hold the same day, the same weeks,
-- and the same primary window. Every episode on the old template therefore still
-- airs at the same instant on the new one.
--
-- This is the alternative to 'clearTemplateForUpcomingEpisodes'. That function
-- detaches an episode, which is correct when the new template does not hold the
-- episode's date. Use this function only when the new template holds every date
-- that the old one held. The gates match the ones that function applies, so the
-- two split the same set of episodes.
migrateUpcomingEpisodes ::
  -- | The template the episodes are on now
  ShowSchedule.TemplateId ->
  -- | The template they move to
  ShowSchedule.TemplateId ->
  -- | The change date. Episodes before it stay on the old template.
  Day ->
  Hasql.Statement () [Id]
migrateUpcomingEpisodes fromTemplateId toTemplateId fromDate =
  interp
    False
    [sql|
    UPDATE episodes
    SET schedule_template_id = #{toTemplateId}, updated_at = NOW()
    WHERE schedule_template_id = #{fromTemplateId}
      AND scheduled_at > NOW()
      AND (scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE >= #{fromDate}
      AND deleted_at IS NULL
    RETURNING id
  |]

-- | Close every open schedule window of a show, and detach its upcoming episodes.
--
-- Used when a show becomes inactive or is soft-deleted. An inactive show must not
-- keep a claim on a time slot. If it keeps one, a later reactivation can put two
-- shows on the same slot, because the conflict check ignores inactive shows.
--
-- The end date is @GREATEST(effective_from, closeDate)@, not @closeDate@. An active
-- window closes on @closeDate@. A pending window starts after @closeDate@, so
-- @closeDate@ alone would make @effective_until@ earlier than @effective_from@. An
-- inverted range makes the show vanish from every query that reads the schedule.
-- @GREATEST@ gives the pending window the empty range @[from, from)@ instead, which
-- is the same shape the pending cancel path already writes.
--
-- Windows that are already closed or already empty are not changed.
--
-- Episodes on those templates are detached in the same statement, so a caller
-- without a transaction cannot leave a closed window with attached episodes. An
-- episode left on a closed window is invisible to 'getCurrentlyAiringEpisode' and
-- airs as silence. Detached episodes keep their audio and show as UNSCHEDULED.
--
-- The rows to detach are read into @to_detach@ before the update runs. @RETURNING@
-- on an @UPDATE@ gives the new values, and the update writes NULL into
-- @scheduled_at@, so returning from the update itself yields a NULL that cannot
-- decode into 'UpcomingEpisodeRef'. Every CTE in one statement reads the same
-- snapshot, so @to_detach@ holds the values from before the write. Data-modifying
-- CTEs run exactly once whether or not the outer query reads them, so @detached@
-- still executes.
--
-- Returns the detached episodes so the caller can report them.
closeSchedulesAndDetachEpisodes :: Shows.Id -> Day -> Hasql.Statement () [UpcomingEpisodeRef]
closeSchedulesAndDetachEpisodes showId closeDate =
  interp
    False
    [sql|
    WITH closed AS (
      UPDATE schedule_template_validity v
      SET effective_until = GREATEST(v.effective_from, #{closeDate})
      FROM schedule_templates st
      WHERE st.id = v.template_id
        AND st.show_id = #{showId}
        AND v.effective_until IS DISTINCT FROM GREATEST(v.effective_from, #{closeDate})
        AND (v.effective_until IS NULL OR v.effective_until > #{closeDate})
        AND (v.effective_until IS NULL OR v.effective_until > v.effective_from)
      RETURNING st.id AS template_id
    ),
    to_detach AS (
      SELECT e.id, e.episode_number, e.scheduled_at
      FROM episodes e
      WHERE e.schedule_template_id IN (SELECT template_id FROM closed)
        AND e.scheduled_at > NOW()
        AND (e.scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE >= #{closeDate}
        AND e.deleted_at IS NULL
    ),
    detached AS (
      UPDATE episodes
      SET schedule_template_id = NULL, scheduled_at = NULL, updated_at = NOW()
      WHERE id IN (SELECT id FROM to_detach)
      RETURNING id
    )
    SELECT id, episode_number, scheduled_at FROM to_detach ORDER BY scheduled_at
  |]

-- | Upcoming, non-deleted episodes attached to any of the given templates, gated
-- by the change's start date.
--
-- These are the rows 'clearTemplateForUpcomingEpisodes' would null when a
-- schedule edit removes or re-keys their slot. It reports which upcoming
-- episodes an edit unscheduled so staff can reschedule them. The @fromDate@ gate
-- and the @scheduled_at > NOW()@ guard match 'clearTemplateForUpcomingEpisodes',
-- so the report equals the set that gets detached. An empty template
-- list matches nothing and returns an empty result.
getUpcomingEpisodesForTemplates :: [ShowSchedule.TemplateId] -> Day -> Hasql.Statement () [UpcomingEpisodeRef]
getUpcomingEpisodesForTemplates templateIds fromDate =
  interp
    False
    [sql|
    SELECT id, episode_number, scheduled_at
    FROM episodes
    WHERE schedule_template_id = ANY(#{templateIds})
      AND scheduled_at > NOW()
      AND (scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE >= #{fromDate}
      AND deleted_at IS NULL
    ORDER BY scheduled_at
  |]

--------------------------------------------------------------------------------
-- Search Queries

-- | Search episodes that have audio, joining with shows for the title.
--
-- Filters by show title ILIKE match, ordered by scheduled_at descending.
-- Used by the force-play admin feature to find episodes to push to the stream.
searchEpisodesWithAudio :: Text -> Hasql.Statement () [SearchResult]
searchEpisodesWithAudio query =
  let pattern = "%" <> query <> "%"
   in interp
        False
        [sql|
    SELECT e.id, s.title, e.episode_number, e.scheduled_at, e.duration_seconds
    FROM episodes e
    JOIN shows s ON s.id = e.show_id
    WHERE e.audio_file_path IS NOT NULL
      AND e.deleted_at IS NULL
      AND s.deleted_at IS NULL
      AND s.title ILIKE #{pattern}
    ORDER BY e.scheduled_at DESC NULLS LAST
    LIMIT 20
  |]

--------------------------------------------------------------------------------
-- Tag Junction Queries

-- | Get all tags for an episode.
getTagsForEpisode :: Id -> Hasql.Statement () [EpisodeTags.Model]
getTagsForEpisode episodeId =
  run $
    select $
      orderBy (EpisodeTags.etName >$< asc) do
        eta <- each episodeTagAssignmentSchema
        where_ $ etaEpisodeId eta ==. lit episodeId
        tag <- each EpisodeTags.episodeTagSchema
        where_ $ EpisodeTags.etId tag ==. etaTagId eta
        pure tag

-- | Replace all tags for an episode with a new set of tags.
--
-- This is an atomic operation that:
-- 1. Upserts tag names into @episode_tags@, returning their IDs
-- 2. Removes assignments for tags no longer in the new set
-- 3. Inserts assignments for tags in the new set (skipping existing ones)
--
-- The DELETE and INSERT target non-overlapping subsets of
-- @episode_tag_assignments@, avoiding the PostgreSQL CTE snapshot issue
-- where a DELETE and INSERT on the same rows within a single statement
-- cannot see each other's effects.
--
-- Pass an empty list to remove all tags.
replaceEpisodeTags :: Id -> [Text] -> Hasql.Statement () ()
replaceEpisodeTags episodeId tagNames =
  interp
    False
    [sql|
    WITH
      -- Upsert tags, using DO UPDATE to return existing ones too
      all_tags AS (
        INSERT INTO episode_tags (name)
        SELECT DISTINCT unnest(#{tagNames}::text[])
        ON CONFLICT (name) DO UPDATE SET name = EXCLUDED.name
        RETURNING id
      ),
      -- Remove assignments for tags NOT in the new set
      removed AS (
        DELETE FROM episode_tag_assignments
        WHERE episode_id = #{episodeId}
        AND tag_id NOT IN (SELECT id FROM all_tags)
      )
    -- Create assignments for new tags (existing ones kept via DO NOTHING)
    INSERT INTO episode_tag_assignments (episode_id, tag_id)
    SELECT #{episodeId}, id
    FROM all_tags
    ON CONFLICT DO NOTHING
  |]
