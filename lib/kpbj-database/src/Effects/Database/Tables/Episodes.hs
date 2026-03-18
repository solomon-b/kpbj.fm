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

    -- * Queries
    getPublishedEpisodesForShow,
    getEpisodesForShow,
    getEpisodeByShowAndNumber,
    getEpisodeById,
    getEpisodeByAudioPath,
    getEpisodesByUser,
    getCurrentlyAiringEpisode,
    insertEpisode,
    updateEpisode,
    updateEpisodeFiles,
    updateScheduledSlot,
    deleteEpisode,
    clearTemplateForUpcomingEpisodes,

    -- * Result Types
    EpisodeWithShow (..),
    SearchResult (..),

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
import Data.Time (UTCTime)
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

-- | Get episodes for a show (for hosts viewing their own show).
--
-- Returns all non-deleted episodes, ordered by scheduled date descending.
getEpisodesForShow :: Shows.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getEpisodesForShow showId' (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy ((.scheduledAt) >$< nullsLast desc) do
            ep <- each episodeSchema
            where_ $ ep.showId ==. lit showId'
            where_ $ isNull ep.deletedAt
            pure ep

-- | Get episode by show slug and episode number.
--
-- Joins with shows table to filter by show slug.
getEpisodeByShowAndNumber :: Slug -> EpisodeNumber -> Hasql.Statement () (Maybe Model)
getEpisodeByShowAndNumber showSlug episodeNum = fmap listToMaybe $ run $ select do
  ep <- each episodeSchema
  s <- each Shows.showSchema
  where_ $ showId ep ==. Shows.id s
  where_ $ Shows.slug s ==. lit showSlug
  where_ $ episodeNumber ep ==. lit episodeNum
  pure ep

-- | Get non-deleted episode by ID.
getEpisodeById :: Id -> Hasql.Statement () (Maybe Model)
getEpisodeById episodeId = fmap listToMaybe $ run $ select do
  ep <- each episodeSchema
  where_ $ ep.id ==. lit episodeId
  where_ $ isNull (ep.deletedAt)
  pure ep

-- | Find a non-deleted episode by its audio file path (object key).
--
-- Used to resolve a media URL back to its episode record by matching
-- the stored @audio_file_path@ column.
getEpisodeByAudioPath :: Text -> Hasql.Statement () (Maybe Model)
getEpisodeByAudioPath audioPath = fmap listToMaybe $ run $ select do
  ep <- each episodeSchema
  where_ $ ep.audioFilePath ==. nullify (lit audioPath)
  where_ $ isNull ep.deletedAt
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
-- * __Case 4__: Replay (replay_start_time), scheduled today, within duration or slot
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
    -- Precompute show duration and replay end time to avoid repeating the
    -- overnight-aware duration CASE expression across every replay case.
    schedule_slots AS (
      SELECT
        st.*,
        e.id AS ep_id,
        e.show_id AS ep_show_id,
        e.description AS ep_description,
        e.episode_number AS ep_episode_number,
        e.audio_file_path AS ep_audio_file_path,
        e.audio_file_size AS ep_audio_file_size,
        e.audio_mime_type AS ep_audio_mime_type,
        e.duration_seconds AS ep_duration_seconds,
        e.artwork_url AS ep_artwork_url,
        e.schedule_template_id AS ep_schedule_template_id,
        e.scheduled_at AS ep_scheduled_at,
        e.published_at AS ep_published_at,
        e.deleted_at AS ep_deleted_at,
        e.created_by AS ep_created_by,
        e.created_at AS ep_created_at,
        e.updated_at AS ep_updated_at,
        -- Show duration as interval (handles overnight wraparound)
        CASE WHEN st.end_time > st.start_time
          THEN st.end_time - st.start_time
          ELSE INTERVAL '24 hours' - (st.start_time - st.end_time)
        END AS show_duration,
        -- Replay end time (NULL when no replay)
        (st.replay_start_time + (
          CASE WHEN st.end_time > st.start_time
            THEN st.end_time - st.start_time
            ELSE INTERVAL '24 hours' - (st.start_time - st.end_time)
          END
        ))::TIME AS replay_end_time
      FROM episodes e
      JOIN schedule_templates st ON st.id = e.schedule_template_id
      JOIN schedule_template_validity stv ON stv.template_id = st.id
      WHERE
        e.audio_file_path IS NOT NULL
        AND e.deleted_at IS NULL
        AND stv.effective_from <= (e.scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE
        AND (stv.effective_until IS NULL OR stv.effective_until > (e.scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE)
    ),
    matching_episodes AS (
      SELECT
        ss.ep_id AS id, ss.ep_show_id AS show_id, ss.ep_description AS description,
        ss.ep_episode_number AS episode_number, ss.ep_audio_file_path AS audio_file_path,
        ss.ep_audio_file_size AS audio_file_size, ss.ep_audio_mime_type AS audio_mime_type,
        ss.ep_duration_seconds AS duration_seconds, ss.ep_artwork_url AS artwork_url,
        ss.ep_schedule_template_id AS schedule_template_id, ss.ep_scheduled_at AS scheduled_at,
        ss.ep_published_at AS published_at, ss.ep_deleted_at AS deleted_at,
        ss.ep_created_by AS created_by, ss.ep_created_at AS created_at, ss.ep_updated_at AS updated_at
      FROM schedule_slots ss
      CROSS JOIN current_pacific cp
      WHERE
        (
          -- Case 1: Standard show (end > start) scheduled for today
          (
            ss.end_time > ss.start_time
            AND (ss.ep_scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE = cp.today_pacific
            AND cp.time_now >= ss.start_time
            AND cp.time_now < LEAST(
              ss.end_time,
              (ss.start_time + COALESCE(ss.ep_duration_seconds * INTERVAL '1 second', ss.show_duration))::TIME
            )
          )
          OR
          -- Case 2: Overnight show (end <= start) - before midnight portion (scheduled today)
          (
            ss.end_time <= ss.start_time
            AND (ss.ep_scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE = cp.today_pacific
            AND cp.time_now >= ss.start_time
            AND (
              ss.ep_duration_seconds IS NULL
              OR ss.ep_duration_seconds >= EXTRACT(EPOCH FROM (TIME '24:00:00' - ss.start_time))
              OR cp.time_now < (ss.start_time + ss.ep_duration_seconds * INTERVAL '1 second')::TIME
            )
          )
          OR
          -- Case 3: Overnight show (end <= start) - after midnight portion (scheduled yesterday)
          (
            ss.end_time <= ss.start_time
            AND (ss.ep_scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE = cp.yesterday_pacific
            AND cp.time_now < LEAST(
              ss.end_time,
              CASE
                WHEN ss.ep_duration_seconds IS NULL THEN ss.end_time
                WHEN ss.ep_duration_seconds > EXTRACT(EPOCH FROM (TIME '24:00:00' - ss.start_time))
                THEN ((ss.ep_duration_seconds - EXTRACT(EPOCH FROM (TIME '24:00:00' - ss.start_time))) * INTERVAL '1 second')::TIME
                ELSE TIME '00:00:00'
              END
            )
          )
          OR
          -- Case 4: Replay airing for standard replay shows scheduled today
          (
            ss.replay_start_time IS NOT NULL
            AND ss.replay_end_time > ss.replay_start_time
            AND (ss.ep_scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE = cp.today_pacific
            AND cp.time_now >= ss.replay_start_time
            AND cp.time_now < LEAST(
              ss.replay_end_time,
              (ss.replay_start_time + COALESCE(ss.ep_duration_seconds * INTERVAL '1 second', ss.show_duration))::TIME
            )
          )
          OR
          -- Case 5: Replay airing for overnight replay shows - before midnight portion (scheduled today)
          (
            ss.replay_start_time IS NOT NULL
            AND ss.replay_end_time <= ss.replay_start_time
            AND (ss.ep_scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE = cp.today_pacific
            AND cp.time_now >= ss.replay_start_time
            AND (
              ss.ep_duration_seconds IS NULL
              OR ss.ep_duration_seconds >= EXTRACT(EPOCH FROM (TIME '24:00:00' - ss.replay_start_time))
              OR cp.time_now < (ss.replay_start_time + ss.ep_duration_seconds * INTERVAL '1 second')::TIME
            )
          )
          OR
          -- Case 6: Replay airing for overnight replay shows - after midnight portion (scheduled yesterday)
          (
            ss.replay_start_time IS NOT NULL
            AND ss.replay_end_time <= ss.replay_start_time
            AND (ss.ep_scheduled_at AT TIME ZONE 'America/Los_Angeles')::DATE = cp.yesterday_pacific
            AND cp.time_now < LEAST(
              ss.replay_end_time,
              CASE
                WHEN ss.ep_duration_seconds IS NULL THEN ss.replay_end_time
                WHEN ss.ep_duration_seconds > EXTRACT(EPOCH FROM (TIME '24:00:00' - ss.replay_start_time))
                THEN ((ss.ep_duration_seconds - EXTRACT(EPOCH FROM (TIME '24:00:00' - ss.replay_start_time))) * INTERVAL '1 second')::TIME
                ELSE TIME '00:00:00'
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

-- | Delete an episode (soft delete by setting deleted_at timestamp).
deleteEpisode :: Id -> Hasql.Statement () (Maybe Id)
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
            returning = Returning (.id)
          }

-- | Clear schedule_template_id for upcoming episodes tied to a given template.
--
-- Used when a schedule template is invalidated (e.g., timeslot changed) to
-- explicitly detach future episodes rather than leaving them with a stale FK.
-- Returns the IDs of affected episodes for logging.
clearTemplateForUpcomingEpisodes :: ShowSchedule.TemplateId -> Hasql.Statement () [Id]
clearTemplateForUpcomingEpisodes templateId =
  interp
    False
    [sql|
    UPDATE episodes
    SET schedule_template_id = NULL, scheduled_at = NULL, updated_at = NOW()
    WHERE schedule_template_id = #{templateId}
      AND scheduled_at > NOW()
      AND deleted_at IS NULL
    RETURNING id
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
