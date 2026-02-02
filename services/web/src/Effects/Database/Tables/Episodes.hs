{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

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

    -- * Insert Type
    Insert (..),

    -- * Update Types
    Update (..),
    FileUpdate (..),
    ScheduleSlotUpdate (..),

    -- * Queries
    getPublishedEpisodesForShow,
    getEpisodesForShow,
    getEpisodeByShowAndNumber,
    getEpisodeById,
    getEpisodesByUser,
    getCurrentlyAiringEpisode,
    insertEpisode,
    updateEpisode,
    updateEpisodeFiles,
    updateScheduledSlot,
    deleteEpisode,

    -- * Result Types
    EpisodeWithShow (..),

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
import Data.Time (UTCTime)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EpisodeTags qualified as EpisodeTags
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Enum, Insert, Update)
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
    scheduleTemplateId :: Column f ShowSchedule.TemplateId,
    scheduledAt :: Column f UTCTime,
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
    eiScheduleTemplateId :: ShowSchedule.TemplateId,
    eiScheduledAt :: UTCTime,
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
    ewsScheduleTemplateId :: ShowSchedule.TemplateId,
    ewsScheduledAt :: UTCTime,
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
            where_ $ ep.scheduledAt <=. lit currentTime
            pure ep

-- | Get episodes for a show (for hosts viewing their own show).
--
-- Returns all non-deleted episodes, ordered by scheduled date descending.
getEpisodesForShow :: Shows.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getEpisodesForShow showId' (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy ((.scheduledAt) >$< desc) do
            ep <- each episodeSchema
            where_ $ ep.showId ==. lit showId'
            where_ $ isNull ep.deletedAt
            pure ep

-- | Get episode by show slug and episode number.
--
-- Uses raw SQL because it requires a JOIN with the shows table.
getEpisodeByShowAndNumber :: Slug -> EpisodeNumber -> Hasql.Statement () (Maybe Model)
getEpisodeByShowAndNumber showSlug episodeNumber =
  interp
    False
    [sql|
    SELECT e.id, e.show_id, e.description, e.episode_number,
           e.audio_file_path, e.audio_file_size, e.audio_mime_type, e.duration_seconds,
           e.artwork_url, e.schedule_template_id, e.scheduled_at, e.published_at, e.deleted_at, e.created_by, e.created_at, e.updated_at
    FROM episodes e
    JOIN shows s ON e.show_id = s.id
    WHERE s.slug = #{showSlug} AND e.episode_number = #{episodeNumber}
  |]

-- | Get episode by ID.
getEpisodeById :: Id -> Hasql.Statement () (Maybe Model)
getEpisodeById episodeId = fmap listToMaybe $ run $ select do
  ep <- each episodeSchema
  where_ $ ep.id ==. lit episodeId
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

-- | Get the episode that is currently airing based on the schedule.
--
-- Finds episodes where:
--
-- 1. The episode has an audio file uploaded
-- 2. The episode is not deleted
-- 3. The current Pacific time falls within the show's airing window
-- 4. The schedule template is currently valid (effective dates match)
--
-- == Duration-Based Airing
--
-- When @duration_seconds@ is set, the episode only airs for that duration
-- (prevents replay bleeding when episode is shorter than time slot).
-- When @duration_seconds@ is NULL, falls back to the full slot duration.
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
-- == The 6 Cases
--
-- Standard shows (end > start):
--
-- * __Case 1__: Primary airing, scheduled today, within duration or slot
-- * __Case 4__: Replay (+12h), scheduled today, within duration or slot
--
-- Overnight shows (end <= start, e.g., 11 PM - 2 AM):
--
-- * __Case 2__: Primary, before midnight portion (scheduled today)
-- * __Case 3__: Primary, after midnight portion (scheduled yesterday)
-- * __Case 5__: Replay, before midnight portion (scheduled today)
-- * __Case 6__: Replay, after midnight portion (scheduled yesterday)
--
-- For overnight shows, duration logic is more complex because it may
-- end before midnight (only Case 2/5 matches) or extend past midnight
-- (both before and after midnight portions match).
--
-- Used by Liquidsoap to determine what audio to play.
getCurrentlyAiringEpisode :: UTCTime -> Hasql.Statement () (Maybe Model)
getCurrentlyAiringEpisode currentTime =
  interp
    False
    [sql|
    WITH current_pacific AS (
      -- Convert current UTC time to Pacific
      SELECT
        (#{currentTime} AT TIME ZONE 'America/Los_Angeles')::DATE as today_pacific,
        ((#{currentTime} AT TIME ZONE 'America/Los_Angeles')::DATE - INTERVAL '1 day')::DATE as yesterday_pacific,
        (#{currentTime} AT TIME ZONE 'America/Los_Angeles')::TIME as time_now
    ),
    matching_episodes AS (
      SELECT
        e.id, e.show_id, e.description, e.episode_number,
        e.audio_file_path, e.audio_file_size, e.audio_mime_type, e.duration_seconds,
        e.artwork_url, e.schedule_template_id, e.scheduled_at, e.published_at,
        e.deleted_at, e.created_by, e.created_at, e.updated_at
      FROM episodes e
      JOIN schedule_templates st ON st.id = e.schedule_template_id
      JOIN schedule_template_validity stv ON stv.template_id = st.id
      CROSS JOIN current_pacific cp
      WHERE
        -- Episode must have audio and not be deleted
        e.audio_file_path IS NOT NULL
        AND e.deleted_at IS NULL
        -- Schedule validity must be active for the episode's scheduled date
        -- effective_from is inclusive, effective_until is exclusive
        AND stv.effective_from <= (e.scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE
        AND (stv.effective_until IS NULL OR stv.effective_until > (e.scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE)
        AND (
          -- Case 1: Standard show (end > start) scheduled for today
          -- Use episode duration as effective end time (fall back to slot end if NULL)
          (
            st.end_time > st.start_time
            AND (e.scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE = cp.today_pacific
            AND cp.time_now >= st.start_time
            AND cp.time_now < LEAST(
              st.end_time,
              (st.start_time + COALESCE(e.duration_seconds * INTERVAL '1 second', st.end_time - st.start_time))::TIME
            )
          )
          OR
          -- Case 2: Overnight show (end <= start) - before midnight portion (scheduled today)
          -- Check if we're past start AND either duration extends past midnight OR we're within duration
          (
            st.end_time <= st.start_time
            AND (e.scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE = cp.today_pacific
            AND cp.time_now >= st.start_time
            AND (
              -- If duration is NULL, allow full before-midnight portion
              e.duration_seconds IS NULL
              -- If duration extends past midnight, allow the entire before-midnight portion
              OR e.duration_seconds >= EXTRACT(EPOCH FROM (TIME '24:00:00' - st.start_time))
              -- If duration ends before midnight, check if we're still within it
              OR cp.time_now < (st.start_time + e.duration_seconds * INTERVAL '1 second')::TIME
            )
          )
          OR
          -- Case 3: Overnight show (end <= start) - after midnight portion (scheduled yesterday)
          -- Check if duration extends past midnight
          (
            st.end_time <= st.start_time
            AND (e.scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE = cp.yesterday_pacific
            AND cp.time_now < LEAST(
              st.end_time,
              -- Duration from start, wrapping past midnight: total duration minus time before midnight
              CASE
                WHEN e.duration_seconds IS NULL THEN st.end_time
                WHEN e.duration_seconds > EXTRACT(EPOCH FROM (TIME '24:00:00' - st.start_time))
                THEN ((e.duration_seconds - EXTRACT(EPOCH FROM (TIME '24:00:00' - st.start_time))) * INTERVAL '1 second')::TIME
                ELSE TIME '00:00:00'  -- Duration ended before midnight; 00:00 < any time, so this case won't match
              END
            )
          )
          OR
          -- Case 4: Replay airing (+12 hours) for standard shows scheduled today
          (
            st.airs_twice_daily = TRUE
            AND (st.end_time + INTERVAL '12 hours')::TIME > (st.start_time + INTERVAL '12 hours')::TIME
            AND (e.scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE = cp.today_pacific
            AND cp.time_now >= (st.start_time + INTERVAL '12 hours')::TIME
            AND cp.time_now < LEAST(
              (st.end_time + INTERVAL '12 hours')::TIME,
              ((st.start_time + INTERVAL '12 hours') + COALESCE(e.duration_seconds * INTERVAL '1 second', st.end_time - st.start_time))::TIME
            )
          )
          OR
          -- Case 5: Replay airing for overnight shows - before midnight portion (scheduled today)
          (
            st.airs_twice_daily = TRUE
            AND (st.end_time + INTERVAL '12 hours')::TIME <= (st.start_time + INTERVAL '12 hours')::TIME
            AND (e.scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE = cp.today_pacific
            AND cp.time_now >= (st.start_time + INTERVAL '12 hours')::TIME
            AND (
              e.duration_seconds IS NULL
              OR cp.time_now < ((st.start_time + INTERVAL '12 hours') + e.duration_seconds * INTERVAL '1 second')::TIME
            )
          )
          OR
          -- Case 6: Replay airing for overnight shows - after midnight portion (scheduled yesterday)
          (
            st.airs_twice_daily = TRUE
            AND (st.end_time + INTERVAL '12 hours')::TIME <= (st.start_time + INTERVAL '12 hours')::TIME
            AND (e.scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE = cp.yesterday_pacific
            AND cp.time_now < LEAST(
              (st.end_time + INTERVAL '12 hours')::TIME,
              CASE
                WHEN e.duration_seconds IS NULL THEN (st.end_time + INTERVAL '12 hours')::TIME
                WHEN e.duration_seconds > EXTRACT(EPOCH FROM (TIME '24:00:00' - (st.start_time + INTERVAL '12 hours')::TIME))
                THEN ((e.duration_seconds - EXTRACT(EPOCH FROM (TIME '24:00:00' - (st.start_time + INTERVAL '12 hours')::TIME))) * INTERVAL '1 second')::TIME
                ELSE TIME '00:00:00'  -- Duration ended before midnight; 00:00 < any time, so this case won't match
              END
            )
          )
        )
    )
    SELECT * FROM matching_episodes
    LIMIT 1
  |]

-- | Insert a new episode.
--
-- Episode numbers are auto-assigned by a PostgreSQL trigger.
-- Episodes are always created as published (published_at = NOW()).
insertEpisode :: Insert -> Hasql.Statement () Id
insertEpisode Insert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO episodes(show_id, description,
                        audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
                        artwork_url, schedule_template_id, scheduled_at, published_at, created_by, created_at, updated_at)
    VALUES (#{eiId}, #{eiDescription},
            #{eiAudioFilePath}, #{eiAudioFileSize}, #{eiAudioMimeType}, #{eiDurationSeconds},
            #{eiArtworkUrl}, #{eiScheduleTemplateId}, #{eiScheduledAt}, NOW(), #{eiCreatedBy}, NOW(), NOW())
    RETURNING id
  |]

-- | Update an episode with partial data (for editing).
--
-- Uses raw SQL because rel8's UPDATE doesn't support partial updates as cleanly.
updateEpisode :: Update -> Hasql.Statement () (Maybe Id)
updateEpisode Update {..} =
  interp
    False
    [sql|
    UPDATE episodes
    SET description = #{euDescription},
        updated_at = NOW()
    WHERE id = #{euId}
    RETURNING id
  |]

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
  interp
    False
    [sql|
    UPDATE episodes
    SET schedule_template_id = #{essuScheduleTemplateId},
        scheduled_at = #{essuScheduledAt},
        updated_at = NOW()
    WHERE id = #{essuId}
    RETURNING id
  |]

-- | Delete an episode (soft delete by setting deleted_at timestamp).
deleteEpisode :: Id -> Hasql.Statement () (Maybe Id)
deleteEpisode episodeId =
  interp
    False
    [sql|
    UPDATE episodes
    SET deleted_at = NOW(), updated_at = NOW()
    WHERE id = #{episodeId}
    RETURNING id
  |]

--------------------------------------------------------------------------------
-- Tag Junction Queries

-- | Get all tags for an episode.
getTagsForEpisode :: Id -> Hasql.Statement () [EpisodeTags.Model]
getTagsForEpisode episodeId =
  interp
    False
    [sql|
    SELECT et.id, et.name, et.created_at
    FROM episode_tags et
    INNER JOIN episode_tag_assignments eta ON et.id = eta.tag_id
    WHERE eta.episode_id = #{episodeId}
    ORDER BY et.name
  |]

-- | Replace all tags for an episode with a new set of tags.
--
-- This is an atomic operation that:
-- 1. Deletes all existing tag assignments for the episode
-- 2. Inserts any new tag names that don't exist yet
-- 3. Creates assignments for all provided tags
--
-- Pass an empty list to remove all tags.
replaceEpisodeTags :: Id -> [Text] -> Hasql.Statement () ()
replaceEpisodeTags episodeId tagNames =
  interp
    False
    [sql|
    WITH
      -- Delete existing assignments
      deleted AS (
        DELETE FROM episode_tag_assignments
        WHERE episode_id = #{episodeId}
      ),
      -- Insert new tags, using DO UPDATE to return existing ones too
      all_tags AS (
        INSERT INTO episode_tags (name)
        SELECT unnest(#{tagNames}::text[])
        ON CONFLICT (name) DO UPDATE SET name = EXCLUDED.name
        RETURNING id
      )
    -- Create assignments using the returned IDs
    INSERT INTO episode_tag_assignments (episode_id, tag_id)
    SELECT #{episodeId}, id
    FROM all_tags
    ON CONFLICT DO NOTHING
  |]
