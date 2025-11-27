{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.Episodes where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..), display, displayBuilder)
import Data.Time (UTCTime)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneColumn (..), OneRow (..), RowsAffected, interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Episode Status Type

data Status = Draft | Scheduled | Published | Archived
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance Display Status where
  displayBuilder Draft = "draft"
  displayBuilder Scheduled = "scheduled"
  displayBuilder Published = "published"
  displayBuilder Archived = "archived"

instance DecodeValue Status where
  decodeValue = Decoders.enum decodeStatus

decodeStatus :: Text -> Maybe Status
decodeStatus = \case
  "draft" -> Just Draft
  "scheduled" -> Just Scheduled
  "published" -> Just Published
  "archived" -> Just Archived
  _ -> Nothing

instance EncodeValue Status where
  encodeValue = Encoders.enum $ \case
    Draft -> "draft"
    Scheduled -> "scheduled"
    Published -> "published"
    Archived -> "archived"

instance Servant.FromHttpApiData Status where
  parseUrlPiece "draft" = Right Draft
  parseUrlPiece "scheduled" = Right Scheduled
  parseUrlPiece "published" = Right Published
  parseUrlPiece "archived" = Right Archived
  parseUrlPiece invalid = Left $ "Invalid Status: " <> invalid

instance Servant.ToHttpApiData Status where
  toUrlPiece = display

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
    showId :: Shows.Id,
    title :: Text,
    slug :: Slug,
    description :: Maybe Text,
    episodeNumber :: EpisodeNumber,
    audioFilePath :: Maybe Text,
    audioFileSize :: Maybe Int64,
    audioMimeType :: Maybe Text,
    durationSeconds :: Maybe Int64,
    artworkUrl :: Maybe Text,
    scheduledAt :: Maybe UTCTime,
    publishedAt :: Maybe UTCTime,
    status :: Status,
    createdBy :: User.Id,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

newtype EpisodeNumber = EpisodeNumber Int64
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

data Insert = Insert
  { eiId :: Shows.Id,
    eiTitle :: Text,
    eiSlug :: Slug,
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
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance Insert)

-- | Episode Update data for partial updates
data Update = Update
  { euId :: Id,
    euTitle :: Text,
    euDescription :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance Update)

--------------------------------------------------------------------------------
-- Database Queries

-- | Get published episodes for a show
getPublishedEpisodesForShow :: Shows.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getPublishedEpisodesForShow showId limit offset =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, description, episode_number,
           audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
           artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at
    FROM episodes
    WHERE show_id = #{showId} AND status = 'published'
    ORDER BY published_at DESC NULLS LAST, episode_number DESC NULLS LAST, created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get all non-archived episodes for a show
getEpisodesById :: Shows.Id -> Hasql.Statement () [Model]
getEpisodesById showId =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, description, episode_number,
           audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
           artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at
    FROM episodes
    WHERE show_id = #{showId} AND status != 'archived'
    ORDER BY published_at DESC NULLS LAST, episode_number DESC NULLS LAST, created_at DESC
  |]

-- | Get all recent episodes across all shows (for admin dashboard)
getAllRecentEpisodes :: Limit -> Hasql.Statement () [Model]
getAllRecentEpisodes limit =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, description, episode_number,
           audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
           artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at
    FROM episodes
    WHERE status != 'archived'
    ORDER BY published_at DESC NULLS LAST, created_at DESC
    LIMIT #{limit}
  |]

-- | Get episode by show slug and episode slug
getEpisodeBySlug :: Slug -> Slug -> Hasql.Statement () (Maybe Model)
getEpisodeBySlug showSlug episodeSlug =
  interp
    False
    [sql|
    SELECT e.id, e.show_id, e.title, e.slug, e.description, e.episode_number,
           e.audio_file_path, e.audio_file_size, e.audio_mime_type, e.duration_seconds,
           e.artwork_url, e.scheduled_at, e.published_at, e.status, e.created_by, e.created_at, e.updated_at
    FROM episodes e
    JOIN shows s ON e.show_id = s.id
    WHERE s.slug = #{showSlug} AND e.slug = #{episodeSlug}
  |]

-- | Get episode by ID
getEpisodeById :: Id -> Hasql.Statement () (Maybe Model)
getEpisodeById episodeId =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, description, episode_number,
           audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
           artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at
    FROM episodes
    WHERE id = #{episodeId}
  |]

-- | Get non-archived episodes by user (episodes they created)
getEpisodesByUser :: User.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getEpisodesByUser userId limit offset =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, description, episode_number,
           audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
           artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at
    FROM episodes
    WHERE created_by = #{userId} AND status != 'archived'
    ORDER BY created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get recent published episodes across all shows
getRecentPublishedEpisodes :: Limit -> Offset -> Hasql.Statement () [Model]
getRecentPublishedEpisodes limit offset =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, description, episode_number,
           audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
           artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at
    FROM episodes
    WHERE status = 'published'
    ORDER BY published_at DESC NULLS LAST, created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Data type for episode archive results with show information
-- This flattens Model fields + show info for easier decoding
data EpisodeWithShow = EpisodeWithShow
  { ewsId :: Id,
    ewsShowId :: Shows.Id,
    ewsTitle :: Text,
    ewsSlug :: Slug,
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

-- | Get published episodes with show details and filters for archive page
-- Returns episodes with show information, filtered by optional search, show_id, genre, and date range
getPublishedEpisodesWithFilters ::
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Text ->
  Limit ->
  Offset ->
  Hasql.Statement () [EpisodeWithShow]
getPublishedEpisodesWithFilters mSearch mGenre mDateFrom mDateTo sortBy limit offset =
  case sortBy of
    "longest" ->
      interp
        False
        [sql|
    SELECT
      e.id, e.show_id, e.title, e.slug, e.description, e.episode_number,
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
      AND (#{mSearch}::text IS NULL OR e.title ILIKE '%' || #{mSearch}::text || '%' OR e.description ILIKE '%' || #{mSearch}::text || '%' OR s.title ILIKE '%' || #{mSearch}::text || '%')
      AND (#{mGenre}::text IS NULL OR s.genre ILIKE #{mGenre}::text)
      AND (#{mDateFrom}::timestamptz IS NULL OR e.published_at >= #{mDateFrom}::timestamptz)
      AND (#{mDateTo}::timestamptz IS NULL OR e.published_at <= #{mDateTo}::timestamptz)
    ORDER BY e.duration_seconds DESC NULLS LAST, e.published_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]
    _ ->
      interp
        False
        [sql|
    SELECT
      e.id, e.show_id, e.title, e.slug, e.description, e.episode_number,
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
      AND (#{mSearch}::text IS NULL OR e.title ILIKE '%' || #{mSearch}::text || '%' OR e.description ILIKE '%' || #{mSearch}::text || '%' OR s.title ILIKE '%' || #{mSearch}::text || '%')
      AND (#{mGenre}::text IS NULL OR s.genre ILIKE #{mGenre}::text)
      AND (#{mDateFrom}::timestamptz IS NULL OR e.published_at >= #{mDateFrom}::timestamptz)
      AND (#{mDateTo}::timestamptz IS NULL OR e.published_at <= #{mDateTo}::timestamptz)
    ORDER BY e.published_at DESC NULLS LAST, e.created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Count published episodes with filters for pagination
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
      AND (#{mSearch}::text IS NULL OR e.title ILIKE '%' || #{mSearch}::text || '%' OR e.description ILIKE '%' || #{mSearch}::text || '%' OR s.title ILIKE '%' || #{mSearch}::text || '%')
      AND (#{mGenre}::text IS NULL OR s.genre ILIKE #{mGenre}::text)
      AND (#{mDateFrom}::timestamptz IS NULL OR e.published_at >= #{mDateFrom}::timestamptz)
      AND (#{mDateTo}::timestamptz IS NULL OR e.published_at <= #{mDateTo}::timestamptz)
  |]

-- | Insert a new episode
insertEpisode :: Insert -> Hasql.Statement () Id
insertEpisode Insert {..} =
  case eiStatus of
    Published ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO episodes(show_id, title, slug, description,
                            audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
                            artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at)
        VALUES (#{eiId}, #{eiTitle}, #{eiSlug}, #{eiDescription},
                #{eiAudioFilePath}, #{eiAudioFileSize}, #{eiAudioMimeType}, #{eiDurationSeconds},
                #{eiArtworkUrl}, #{eiScheduledAt}, NOW(), #{eiStatus}, #{eiCreatedBy}, NOW(), NOW())
        RETURNING id
      |]
    _ ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO episodes(show_id, title, slug, description,
                            audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
                            artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at)
        VALUES (#{eiId}, #{eiTitle}, #{eiSlug}, #{eiDescription},
                #{eiAudioFilePath}, #{eiAudioFileSize}, #{eiAudioMimeType}, #{eiDurationSeconds},
                #{eiArtworkUrl}, #{eiScheduledAt}, NULL, #{eiStatus}, #{eiCreatedBy}, NOW(), NOW())
        RETURNING id
      |]

-- | Update an episode with partial data (for editing)
updateEpisode :: Update -> Hasql.Statement () (Maybe Id)
updateEpisode Update {..} =
  interp
    False
    [sql|
    UPDATE episodes
    SET title = #{euTitle}, description = #{euDescription},
        updated_at = NOW()
    WHERE id = #{euId}
    RETURNING id
  |]

-- | Archive an episode (soft delete by setting status to archived)
archiveEpisode :: Id -> Hasql.Statement () (Maybe Id)
archiveEpisode episodeId =
  interp
    False
    [sql|
    UPDATE episodes
    SET status = 'archived', updated_at = NOW()
    WHERE id = #{episodeId}
    RETURNING id
  |]

-- | Check if a user is a current host of the show for a given episode
-- Returns True if the user is a current host, False otherwise
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

-- | Hard delete an episode (use with caution - prefer archiveEpisode for soft delete)
deleteEpisode :: Id -> Hasql.Statement () (Maybe Id)
deleteEpisode episodeId =
  interp
    False
    [sql|
    DELETE FROM episodes
    WHERE id = #{episodeId}
    RETURNING id
  |]

--------------------------------------------------------------------------------
-- Episode Track Queries

-- | Get tracks for an episode
getTracksForEpisode :: Id -> Hasql.Statement () [EpisodeTrack.Model]
getTracksForEpisode episodeId =
  interp
    False
    [sql|
    SELECT id, episode_id, track_number, title, artist, album, year, duration, label, is_exclusive_premiere, created_at
    FROM episode_tracks
    WHERE episode_id = #{episodeId}
    ORDER BY track_number
  |]

-- | Insert a new episode track
insertEpisodeTrack :: EpisodeTrack.Insert -> Hasql.Statement () EpisodeTrack.Id
insertEpisodeTrack EpisodeTrack.Insert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO episode_tracks(episode_id, track_number, title, artist, album, year, duration, label, is_exclusive_premiere, created_at)
    VALUES (#{etiEpisodeId}, #{etiTrackNumber}, #{etiTitle}, #{etiArtist}, #{etiAlbum}, #{etiYear}, #{etiDuration}, #{etiLabel}, #{etiIsExclusivePremiere}, NOW())
    RETURNING id
  |]

-- | Update an episode track
updateEpisodeTrack :: EpisodeTrack.Id -> EpisodeTrack.Insert -> Hasql.Statement () (Maybe EpisodeTrack.Id)
updateEpisodeTrack trackId EpisodeTrack.Insert {..} =
  interp
    False
    [sql|
    UPDATE episode_tracks
    SET track_number = #{etiTrackNumber}, title = #{etiTitle}, artist = #{etiArtist},
        album = #{etiAlbum}, year = #{etiYear}, duration = #{etiDuration},
        label = #{etiLabel}, is_exclusive_premiere = #{etiIsExclusivePremiere}
    WHERE id = #{trackId}
    RETURNING id
  |]

-- | Delete an episode track
deleteEpisodeTrack :: EpisodeTrack.Id -> Hasql.Statement () (Maybe EpisodeTrack.Id)
deleteEpisodeTrack trackId =
  interp
    False
    [sql|
    DELETE FROM episode_tracks
    WHERE id = #{trackId}
    RETURNING id
  |]

-- | Delete all tracks for an episode
deleteAllTracksForEpisode :: Id -> Hasql.Statement () RowsAffected
deleteAllTracksForEpisode episodeId =
  interp
    False
    [sql|
    DELETE FROM episode_tracks
    WHERE episode_id = #{episodeId}
  |]
