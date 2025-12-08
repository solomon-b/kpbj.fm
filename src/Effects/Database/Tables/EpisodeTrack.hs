{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @episode_tracks@.
module Effects.Database.Tables.EpisodeTrack
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    EpisodeTrack (..),
    episodeTrackSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getTracksForEpisode,
    insertEpisodeTrack,
    updateEpisodeTrack,
    deleteEpisodeTrack,
    deleteAllTracksForEpisode,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display (..), RecordInstance (..))
import Data.Time (UTCTime)
import Effects.Database.Tables.Episodes qualified as Episodes
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneRow (..), RowsAffected, interp, sql)
import Hasql.Statement qualified as Hasql
import Rel8 (Column, DBEq, DBOrd, DBType, Name, Rel8able, Result, TableSchema (..))
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for episode track primary keys.
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
-- Table Definition

-- | The @episode_tracks@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data EpisodeTrack f = EpisodeTrack
  { id :: Column f Id,
    episodeId :: Column f Episodes.Id,
    trackNumber :: Column f Int64,
    title :: Column f Text,
    artist :: Column f Text,
    album :: Column f (Maybe Text),
    year :: Column f (Maybe Int64),
    duration :: Column f (Maybe Text),
    label :: Column f (Maybe Text),
    isExclusivePremiere :: Column f Bool,
    createdAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (EpisodeTrack f)

deriving stock instance (f ~ Result) => Eq (EpisodeTrack f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (EpisodeTrack Result)

-- | Display instance for EpisodeTrack Result.
instance Display (EpisodeTrack Result) where
  displayBuilder track =
    "EpisodeTrack { id = "
      <> displayBuilder track.id
      <> ", title = "
      <> displayBuilder track.title
      <> ", artist = "
      <> displayBuilder track.artist
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @EpisodeTrack Result@.
type Model = EpisodeTrack Result

-- | Table schema connecting the Haskell type to the database table.
episodeTrackSchema :: TableSchema (EpisodeTrack Name)
episodeTrackSchema =
  TableSchema
    { name = "episode_tracks",
      columns =
        EpisodeTrack
          { id = "id",
            episodeId = "episode_id",
            trackNumber = "track_number",
            title = "title",
            artist = "artist",
            album = "album",
            year = "year",
            duration = "duration",
            label = "label",
            isExclusivePremiere = "is_exclusive_premiere",
            createdAt = "created_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new episode tracks.
data Insert = Insert
  { etiEpisodeId :: Episodes.Id,
    etiTrackNumber :: Int64,
    etiTitle :: Text,
    etiArtist :: Text,
    etiAlbum :: Maybe Text,
    etiYear :: Maybe Int64,
    etiDuration :: Maybe Text,
    etiLabel :: Maybe Text,
    etiIsExclusivePremiere :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Queries

-- | Get tracks for an episode.
getTracksForEpisode :: Episodes.Id -> Hasql.Statement () [Model]
getTracksForEpisode episodeId =
  interp
    False
    [sql|
    SELECT id, episode_id, track_number, title, artist, album, year, duration, label, is_exclusive_premiere, created_at
    FROM episode_tracks
    WHERE episode_id = #{episodeId}
    ORDER BY track_number
  |]

-- | Insert a new episode track.
insertEpisodeTrack :: Insert -> Hasql.Statement () Id
insertEpisodeTrack Insert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO episode_tracks(episode_id, track_number, title, artist, album, year, duration, label, is_exclusive_premiere, created_at)
    VALUES (#{etiEpisodeId}, #{etiTrackNumber}, #{etiTitle}, #{etiArtist}, #{etiAlbum}, #{etiYear}, #{etiDuration}, #{etiLabel}, #{etiIsExclusivePremiere}, NOW())
    RETURNING id
  |]

-- | Update an episode track.
updateEpisodeTrack :: Id -> Insert -> Hasql.Statement () (Maybe Id)
updateEpisodeTrack trackId Insert {..} =
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

-- | Delete an episode track.
deleteEpisodeTrack :: Id -> Hasql.Statement () (Maybe Id)
deleteEpisodeTrack trackId =
  interp
    False
    [sql|
    DELETE FROM episode_tracks
    WHERE id = #{trackId}
    RETURNING id
  |]

-- | Delete all tracks for an episode.
deleteAllTracksForEpisode :: Episodes.Id -> Hasql.Statement () RowsAffected
deleteAllTracksForEpisode episodeId =
  interp
    False
    [sql|
    DELETE FROM episode_tracks
    WHERE episode_id = #{episodeId}
  |]
