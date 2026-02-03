{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @playback_history@.
--
-- Stores a log of what has been played on the stream. Populated by Liquidsoap
-- via POST /api/playout/played when tracks start playing.
module Effects.Database.Tables.PlaybackHistory
  ( -- * Model
    Model (..),

    -- * Insert Type
    Insert (..),

    -- * Queries
    insertPlayback,
    getRecentPlayback,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, EncodeRow, interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()

--------------------------------------------------------------------------------
-- Model

-- | Playback history entry from @playback_history@ table.
data Model = Model
  { phId :: Int64,
    phTitle :: Text,
    phArtist :: Maybe Text,
    phSourceType :: Text,
    phSourceUrl :: Text,
    phStartedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating a new playback history entry.
data Insert = Insert
  { piTitle :: Text,
    piArtist :: Maybe Text,
    piSourceType :: Text,
    piSourceUrl :: Text,
    piStartedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)

--------------------------------------------------------------------------------
-- Queries

-- | Insert a new playback history entry.
insertPlayback :: Insert -> Hasql.Statement () ()
insertPlayback Insert {..} =
  interp
    False
    [sql|
    INSERT INTO playback_history (title, artist, source_type, source_url, started_at)
    VALUES (#{piTitle}, #{piArtist}, #{piSourceType}, #{piSourceUrl}, #{piStartedAt})
  |]

-- | Get the most recent playback history entries.
--
-- Returns entries ordered by started_at descending (most recent first).
getRecentPlayback :: Int64 -> Hasql.Statement () [Model]
getRecentPlayback limit =
  interp
    False
    [sql|
    SELECT id, title, artist, source_type, source_url, started_at
    FROM playback_history
    ORDER BY started_at DESC
    LIMIT #{limit}
  |]
