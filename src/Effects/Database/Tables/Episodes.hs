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

    -- * Queries
    getPublishedEpisodesForShow,
    getEpisodesForShowIncludingDrafts,
    getAllEpisodes,
    getEpisodesById,
    getEpisodeByShowAndNumber,
    getEpisodeById,
    getEpisodesByUser,
    getRecentPublishedEpisodes,
    insertEpisode,
    updateEpisode,
    updateEpisodeFiles,
    deleteEpisode,
    publishEpisode,
    isUserHostOfEpisodeShow,
    hardDeleteEpisode,

    -- * Complex Queries (raw SQL)
    getPublishedEpisodesWithFilters,
    countPublishedEpisodesWithFilters,

    -- * Result Types
    EpisodeWithShow (..),
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
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneColumn (..), OneRow (..), interp, sql)
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
    scheduledAt :: Column f (Maybe UTCTime),
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
    eiScheduledAt :: Maybe UTCTime,
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
data FileUpdate = FileUpdate
  { efuId :: Id,
    efuAudioFilePath :: Maybe Text,
    efuArtworkUrl :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance FileUpdate)

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
    ewsScheduledAt :: Maybe UTCTime,
    ewsPublishedAt :: Maybe UTCTime,
    ewsStatus :: Status,
    ewsCreatedBy :: User.Id,
    ewsCreatedAt :: UTCTime,
    ewsUpdatedAt :: UTCTime,
    ewsShowTitle :: Text,
    ewsShowSlug :: Slug,
    ewsShowGenre :: Maybe Text,
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
            where_ $ ep.scheduledAt <=. nullify (lit currentTime)
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
          orderBy ((.scheduledAt) >$< nullsLast desc) do
            ep <- each episodeSchema
            where_ $ ep.showId ==. lit showId'
            where_ $ ep.status /=. lit Deleted
            pure ep

-- | Get all non-deleted episodes.
getAllEpisodes :: Hasql.Statement () [Model]
getAllEpisodes =
  run $
    select $
      orderBy ((.scheduledAt) >$< nullsLast desc) do
        each episodeSchema

-- | Get all non-deleted episodes for a show.
getEpisodesById :: Shows.Id -> Hasql.Statement () [Model]
getEpisodesById showId' =
  run $
    select $
      orderBy ((.scheduledAt) >$< nullsLast desc) do
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
           e.artwork_url, e.scheduled_at, e.published_at, e.status, e.created_by, e.created_at, e.updated_at
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

-- | Get recent published episodes across all shows.
getRecentPublishedEpisodes :: Limit -> Offset -> Hasql.Statement () [Model]
getRecentPublishedEpisodes (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy ((.publishedAt) >$< nullsLast desc) do
            ep <- each episodeSchema
            where_ $ ep.status ==. lit Published
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
                            artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at)
        VALUES (#{eiId}, #{eiDescription},
                #{eiAudioFilePath}, #{eiAudioFileSize}, #{eiAudioMimeType}, #{eiDurationSeconds},
                #{eiArtworkUrl}, #{eiScheduledAt}, NOW(), #{eiStatus}, #{eiCreatedBy}, NOW(), NOW())
        RETURNING id
      |]
    _ ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO episodes(show_id, description,
                            audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
                            artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at)
        VALUES (#{eiId}, #{eiDescription},
                #{eiAudioFilePath}, #{eiAudioFileSize}, #{eiAudioMimeType}, #{eiDurationSeconds},
                #{eiArtworkUrl}, #{eiScheduledAt}, NULL, #{eiStatus}, #{eiCreatedBy}, NOW(), NOW())
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
-- Only updates fields that are provided (Just value). Nothing values are ignored
-- and the existing values are kept.
updateEpisodeFiles :: FileUpdate -> Hasql.Statement () (Maybe Id)
updateEpisodeFiles FileUpdate {..} =
  interp
    False
    [sql|
    UPDATE episodes
    SET audio_file_path = COALESCE(#{efuAudioFilePath}, audio_file_path),
        artwork_url = COALESCE(#{efuArtworkUrl}, artwork_url),
        updated_at = NOW()
    WHERE id = #{efuId}
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

-- | Check if a user is a current host of the show for a given episode.
--
-- Returns True if the user is a current host, False otherwise.
-- Uses raw SQL because it requires JOINs with show_hosts table.
isUserHostOfEpisodeShow :: User.Id -> Id -> Hasql.Statement () Bool
isUserHostOfEpisodeShow userId episodeId =
  let query =
        interp
          True
          [sql|
        SELECT EXISTS (
          SELECT 1 FROM show_hosts sh
          JOIN episodes e ON sh.show_id = e.show_id
          WHERE e.id = #{episodeId}
            AND sh.user_id = #{userId}
            AND sh.left_at IS NULL
        )
      |]
   in maybe False getOneColumn <$> query

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
-- Complex Queries (raw SQL)
--
-- These use raw SQL because they involve complex joins, aggregation,
-- or dynamic filtering that would be verbose to express in rel8.

-- | Get published episodes with show details and filters for archive page.
--
-- Returns episodes with show information, filtered by optional search, show_id, genre, and date range.
getPublishedEpisodesWithFilters ::
  UTCTime ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Text ->
  Limit ->
  Offset ->
  Hasql.Statement () [EpisodeWithShow]
getPublishedEpisodesWithFilters currentTime mSearch mGenre mDateFrom mDateTo sortBy (Limit lim) (Offset off) =
  case sortBy of
    "longest" ->
      interp
        False
        [sql|
    SELECT
      e.id, e.show_id, e.description, e.episode_number,
      e.audio_file_path, e.audio_file_size, e.audio_mime_type, e.duration_seconds,
      e.artwork_url, e.scheduled_at, e.published_at, e.status, e.created_by,
      e.created_at, e.updated_at,
      s.title, s.slug, s.genre,
      COALESCE(um.display_name, u.email)
    FROM episodes e
    JOIN shows s ON e.show_id = s.id
    JOIN show_hosts sh ON s.id = sh.show_id AND sh.is_primary = true AND sh.left_at IS NULL
    JOIN users u ON sh.user_id = u.id
    LEFT JOIN user_metadata um ON u.id = um.user_id
    WHERE e.status = 'published'
      AND e.scheduled_at <= #{currentTime}
      AND (#{mSearch}::text IS NULL OR e.description ILIKE '%' || #{mSearch}::text || '%' OR s.title ILIKE '%' || #{mSearch}::text || '%')
      AND (#{mGenre}::text IS NULL OR s.genre ILIKE #{mGenre}::text)
      AND (#{mDateFrom}::timestamptz IS NULL OR e.published_at >= #{mDateFrom}::timestamptz)
      AND (#{mDateTo}::timestamptz IS NULL OR e.published_at <= #{mDateTo}::timestamptz)
    ORDER BY e.duration_seconds DESC NULLS LAST, e.published_at DESC
    LIMIT #{lim} OFFSET #{off}
  |]
    _ ->
      interp
        False
        [sql|
    SELECT
      e.id, e.show_id, e.description, e.episode_number,
      e.audio_file_path, e.audio_file_size, e.audio_mime_type, e.duration_seconds,
      e.artwork_url, e.scheduled_at, e.published_at, e.status, e.created_by,
      e.created_at, e.updated_at,
      s.title, s.slug, s.genre,
      COALESCE(um.display_name, u.email)
    FROM episodes e
    JOIN shows s ON e.show_id = s.id
    JOIN show_hosts sh ON s.id = sh.show_id AND sh.is_primary = true AND sh.left_at IS NULL
    JOIN users u ON sh.user_id = u.id
    LEFT JOIN user_metadata um ON u.id = um.user_id
    WHERE e.status = 'published'
      AND e.scheduled_at <= #{currentTime}
      AND (#{mSearch}::text IS NULL OR e.description ILIKE '%' || #{mSearch}::text || '%' OR s.title ILIKE '%' || #{mSearch}::text || '%')
      AND (#{mGenre}::text IS NULL OR s.genre ILIKE #{mGenre}::text)
      AND (#{mDateFrom}::timestamptz IS NULL OR e.published_at >= #{mDateFrom}::timestamptz)
      AND (#{mDateTo}::timestamptz IS NULL OR e.published_at <= #{mDateTo}::timestamptz)
    ORDER BY e.published_at DESC NULLS LAST, e.created_at DESC
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Count published episodes with filters for pagination.
countPublishedEpisodesWithFilters ::
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Hasql.Statement () Int64
countPublishedEpisodesWithFilters mSearch mGenre mDateFrom mDateTo =
  maybe 0 getOneColumn
    <$> interp
      False
      [sql|
    SELECT COUNT(*)
    FROM episodes e
    JOIN shows s ON e.show_id = s.id
    WHERE e.status = 'published'
      AND (#{mSearch}::text IS NULL OR e.description ILIKE '%' || #{mSearch}::text || '%' OR s.title ILIKE '%' || #{mSearch}::text || '%')
      AND (#{mGenre}::text IS NULL OR s.genre ILIKE #{mGenre}::text)
      AND (#{mDateFrom}::timestamptz IS NULL OR e.published_at >= #{mDateFrom}::timestamptz)
      AND (#{mDateTo}::timestamptz IS NULL OR e.published_at <= #{mDateTo}::timestamptz)
  |]
