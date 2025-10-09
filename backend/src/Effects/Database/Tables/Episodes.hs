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
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneRow (..), RowsAffected, interp, sql)
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
    slug :: Text,
    description :: Maybe Text,
    episodeNumber :: Maybe EpisodeNumber,
    seasonNumber :: Int64,
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
    eiSlug :: Text,
    eiDescription :: Maybe Text,
    eiEpisodeNumber :: Maybe EpisodeNumber,
    eiSeasonNumber :: Int64,
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
    euDescription :: Maybe Text,
    euEpisodeNumber :: Maybe EpisodeNumber,
    euSeasonNumber :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance Update)

--------------------------------------------------------------------------------
-- Database Queries

-- | Get published episodes for a show
getPublishedEpisodesForShow :: Shows.Id -> Int64 -> Int64 -> Hasql.Statement () [Model]
getPublishedEpisodesForShow showId limit offset =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, description, episode_number, season_number,
           audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
           artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at
    FROM episodes
    WHERE show_id = #{showId} AND status = 'published'
    ORDER BY published_at DESC NULLS LAST, episode_number DESC NULLS LAST, created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get all episodes for a show (any status)
getEpisodesById :: Shows.Id -> Hasql.Statement () [Model]
getEpisodesById showId =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, description, episode_number, season_number,
           audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
           artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at
    FROM episodes
    WHERE show_id = #{showId}
    ORDER BY published_at DESC NULLS LAST, episode_number DESC NULLS LAST, created_at DESC
  |]

-- | Get episode by show slug and episode slug
getEpisodeBySlug :: Text -> Text -> Hasql.Statement () (Maybe Model)
getEpisodeBySlug showSlug episodeSlug =
  interp
    False
    [sql|
    SELECT e.id, e.show_id, e.title, e.slug, e.description, e.episode_number, e.season_number,
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
    SELECT id, show_id, title, slug, description, episode_number, season_number,
           audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
           artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at
    FROM episodes
    WHERE id = #{episodeId}
  |]

-- | Get episodes by user (episodes they created)
getEpisodesByUser :: User.Id -> Int64 -> Int64 -> Hasql.Statement () [Model]
getEpisodesByUser userId limit offset =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, description, episode_number, season_number,
           audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
           artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at
    FROM episodes
    WHERE created_by = #{userId}
    ORDER BY created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get recent published episodes across all shows
getRecentPublishedEpisodes :: Int64 -> Int64 -> Hasql.Statement () [Model]
getRecentPublishedEpisodes limit offset =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, description, episode_number, season_number,
           audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
           artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at
    FROM episodes
    WHERE status = 'published'
    ORDER BY published_at DESC NULLS LAST, created_at DESC
    LIMIT #{limit} OFFSET #{offset}
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
        INSERT INTO episodes(show_id, title, slug, description, episode_number, season_number,
                            audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
                            artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at)
        VALUES (#{eiId}, #{eiTitle}, #{eiSlug}, #{eiDescription}, #{eiEpisodeNumber}, #{eiSeasonNumber},
                #{eiAudioFilePath}, #{eiAudioFileSize}, #{eiAudioMimeType}, #{eiDurationSeconds},
                #{eiArtworkUrl}, #{eiScheduledAt}, NOW(), #{eiStatus}, #{eiCreatedBy}, NOW(), NOW())
        RETURNING id
      |]
    _ ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO episodes(show_id, title, slug, description, episode_number, season_number,
                            audio_file_path, audio_file_size, audio_mime_type, duration_seconds,
                            artwork_url, scheduled_at, published_at, status, created_by, created_at, updated_at)
        VALUES (#{eiId}, #{eiTitle}, #{eiSlug}, #{eiDescription}, #{eiEpisodeNumber}, #{eiSeasonNumber},
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
        episode_number = #{euEpisodeNumber}, season_number = #{euSeasonNumber},
        updated_at = NOW()
    WHERE id = #{euId}
    RETURNING id
  |]

-- | Delete an episode
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

-- | Get next episode number for a show
getNextEpisodeNumber :: Shows.Id -> Int64 -> Hasql.Statement () (Maybe EpisodeNumber)
getNextEpisodeNumber showId seasonNumber =
  interp
    True
    [sql|
    SELECT COALESCE(MAX(episode_number), 0) + 1
    FROM episodes
    WHERE show_id = #{showId} AND season_number = #{seasonNumber}
  |]
