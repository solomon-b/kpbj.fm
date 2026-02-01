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

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getTracksForEpisode,
    insertEpisodeTrack,
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
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneRow (..), RowsAffected (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Rel8 (Column, DBEq, DBOrd, DBType, Rel8able, Result)
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

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new episode tracks.
data Insert = Insert
  { etiEpisodeId :: Episodes.Id,
    etiTrackNumber :: Int64,
    etiTitle :: Text,
    etiArtist :: Text
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
    SELECT id, episode_id, track_number, title, artist, created_at
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
    INSERT INTO episode_tracks(episode_id, track_number, title, artist, created_at)
    VALUES (#{etiEpisodeId}, #{etiTrackNumber}, #{etiTitle}, #{etiArtist}, NOW())
    RETURNING id
  |]

-- | Delete all tracks for an episode.
--
-- Returns the number of rows deleted.
deleteAllTracksForEpisode :: Episodes.Id -> Hasql.Statement () Int
deleteAllTracksForEpisode episodeId =
  fromIntegral . unRowsAffected
    <$> interp
      False
      [sql|
    DELETE FROM episode_tracks
    WHERE episode_id = #{episodeId}
  |]
  where
    unRowsAffected (RowsAffected n) = n
