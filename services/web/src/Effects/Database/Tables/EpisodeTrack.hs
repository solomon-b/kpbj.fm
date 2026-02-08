{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
    deleteAllTracksForEpisode,
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
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..))
import Hasql.Statement qualified as Hasql
import Rel8 hiding (Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
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
    etiArtist :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Queries

-- | Get tracks for an episode.
getTracksForEpisode :: Episodes.Id -> Hasql.Statement () [Model]
getTracksForEpisode epId =
  run $
    select $
      orderBy ((.trackNumber) >$< asc) do
        track <- each episodeTrackSchema
        where_ $ (.episodeId) track ==. lit epId
        pure track

-- | Insert a new episode track.
insertEpisodeTrack :: Insert -> Hasql.Statement () (Maybe Id)
insertEpisodeTrack Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = episodeTrackSchema,
            rows =
              values
                [ EpisodeTrack
                    { id = nextId "episode_tracks_id_seq",
                      episodeId = lit etiEpisodeId,
                      trackNumber = lit etiTrackNumber,
                      title = lit etiTitle,
                      artist = lit etiArtist,
                      createdAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning (.id)
          }

-- | Delete all tracks for an episode.
--
-- Returns the number of rows deleted.
deleteAllTracksForEpisode :: Episodes.Id -> Hasql.Statement () Int
deleteAllTracksForEpisode epId =
  fmap length $
    run $
      delete
        Rel8.Delete
          { from = episodeTrackSchema,
            using = pure (),
            deleteWhere = \_ track -> (.episodeId) track ==. lit epId,
            returning = Returning (.id)
          }
