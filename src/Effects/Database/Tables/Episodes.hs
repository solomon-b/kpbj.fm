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

    -- * Status Type
    Status (..),

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
    getEpisodesForShowIncludingDrafts,
    getEpisodeByShowAndNumber,
    getEpisodeById,
    getEpisodesByUser,
    insertEpisode,
    updateEpisode,
    updateEpisodeFiles,
    updateScheduledSlot,
    deleteEpisode,
    publishEpisode,
    hardDeleteEpisode,

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
import Data.Text qualified as Text
import Data.Text.Display (Display (..), RecordInstance (..), display)
import Data.Time (UTCTime)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EpisodeTags qualified as EpisodeTags
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Enum, Insert, Update)
import Servant qualified

--------------------------------------------------------------------------------
-- Episode Status Type

-- | Episode publication status.
data Status = Draft | Published | Deleted
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded, Read)
  deriving anyclass (FromJSON, ToJSON)

instance DBType Status where
  typeInformation :: TypeInformation Status
  typeInformation =
    parseTypeInformation
      ( \case
          "draft" -> Right Draft
          "published" -> Right Published
          "deleted" -> Right Deleted
          other -> Left $ "Invalid Status: " <> Text.unpack other
      )
      ( \case
          Draft -> "draft"
          Published -> "published"
          Deleted -> "deleted"
      )
      typeInformation

instance DBEq Status

instance Display Status where
  displayBuilder Draft = "draft"
  displayBuilder Published = "published"
  displayBuilder Deleted = "deleted"

instance DecodeValue Status where
  decodeValue = Decoders.enum decodeStatus

decodeStatus :: Text -> Maybe Status
decodeStatus = \case
  "draft" -> Just Draft
  "published" -> Just Published
  "deleted" -> Just Deleted
  _ -> Nothing

instance EncodeValue Status where
  encodeValue = Encoders.enum $ \case
    Draft -> "draft"
    Published -> "published"
    Deleted -> "deleted"

instance Servant.FromHttpApiData Status where
  parseUrlPiece "draft" = Right Draft
  parseUrlPiece "published" = Right Published
  parseUrlPiece "deleted" = Right Deleted
  parseUrlPiece invalid = Left $ "Invalid Status: " <> invalid

instance Servant.ToHttpApiData Status where
  toUrlPiece = display

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
    status :: Column f Status,
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
            status = "status",
            createdBy = "created_by",
            createdAt = "created_at",
            updatedAt = "updated_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new episodes.
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
    eiStatus :: Status,
    eiCreatedBy :: User.Id
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Update Types

-- | Episode Update data for partial updates.
data Update = Update
  { euId :: Id,
    euDescription :: Maybe Text,
    euStatus :: Maybe Status
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
    ewsStatus :: Status,
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

-- | Get published episodes for a show.
getPublishedEpisodesForShow :: UTCTime -> Shows.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getPublishedEpisodesForShow currentTime showId' (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy ((.publishedAt) >$< nullsLast desc) do
            ep <- each episodeSchema
            where_ $ ep.showId ==. lit showId'
            where_ $ ep.status ==. lit Published
            where_ $ ep.scheduledAt <=. lit currentTime
            pure ep

-- | Get episodes for a show including drafts (for hosts viewing their own show).
--
-- Returns both published and draft episodes, ordered by scheduled date descending.
-- Excludes deleted episodes.
getEpisodesForShowIncludingDrafts :: Shows.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getEpisodesForShowIncludingDrafts showId' (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy ((.scheduledAt) >$< desc) do
            ep <- each episodeSchema
            where_ $ ep.showId ==. lit showId'
            where_ $ ep.status /=. lit Deleted
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
           e.artwork_url, e.schedule_template_id, e.scheduled_at, e.published_at, e.status, e.created_by, e.created_at, e.updated_at
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
            where_ $ ep.status /=. lit Deleted
            pure ep

-- | Insert a new episode.
--
-- Episode numbers are auto-assigned by a PostgreSQL trigger.
insertEpisode :: Insert -> Hasql.Statement () Id
insertEpisode Insert {..} =
  case eiStatus of
    Published ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO episodes(show_id, description,
                            audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
                            artwork_url, schedule_template_id, scheduled_at, published_at, status, created_by, created_at, updated_at)
        VALUES (#{eiId}, #{eiDescription},
                #{eiAudioFilePath}, #{eiAudioFileSize}, #{eiAudioMimeType}, #{eiDurationSeconds},
                #{eiArtworkUrl}, #{eiScheduleTemplateId}, #{eiScheduledAt}, NOW(), #{eiStatus}, #{eiCreatedBy}, NOW(), NOW())
        RETURNING id
      |]
    _ ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO episodes(show_id, description,
                            audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
                            artwork_url, schedule_template_id, scheduled_at, published_at, status, created_by, created_at, updated_at)
        VALUES (#{eiId}, #{eiDescription},
                #{eiAudioFilePath}, #{eiAudioFileSize}, #{eiAudioMimeType}, #{eiDurationSeconds},
                #{eiArtworkUrl}, #{eiScheduleTemplateId}, #{eiScheduledAt}, NULL, #{eiStatus}, #{eiCreatedBy}, NOW(), NOW())
        RETURNING id
      |]

-- | Update an episode with partial data (for editing).
--
-- Uses raw SQL because rel8's UPDATE doesn't support partial updates as cleanly.
-- Uses COALESCE to only update status when provided (Nothing keeps existing value).
updateEpisode :: Update -> Hasql.Statement () (Maybe Id)
updateEpisode Update {..} =
  interp
    False
    [sql|
    UPDATE episodes
    SET description = #{euDescription},
        status = COALESCE(#{euStatus}, status),
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

-- | Delete an episode (soft delete by setting status to deleted).
deleteEpisode :: Id -> Hasql.Statement () (Maybe Id)
deleteEpisode episodeId =
  interp
    False
    [sql|
    UPDATE episodes
    SET status = 'deleted', updated_at = NOW()
    WHERE id = #{episodeId}
    RETURNING id
  |]

-- | Publish an episode (set status to published and set published_at timestamp).
publishEpisode :: Id -> Hasql.Statement () (Maybe Id)
publishEpisode episodeId =
  interp
    False
    [sql|
    UPDATE episodes
    SET status = 'published', published_at = NOW(), updated_at = NOW()
    WHERE id = #{episodeId}
    RETURNING id
  |]

-- | Hard delete an episode (use with caution - prefer deleteEpisode for soft delete).
hardDeleteEpisode :: Id -> Hasql.Statement () (Maybe Id)
hardDeleteEpisode episodeId =
  fmap listToMaybe $
    run $
      delete
        Rel8.Delete
          { from = episodeSchema,
            using = pure (),
            deleteWhere = \_ ep -> ep.id ==. lit episodeId,
            returning = Returning (.id)
          }

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
