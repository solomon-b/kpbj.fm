{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @playback_history@.
--
-- Stores a log of what has been played on the stream. Populated by Liquidsoap
-- via POST /api/playout/played when tracks start playing.
module Effects.Database.Tables.PlaybackHistory
  ( -- * Table Definition
    PlaybackEntry (..),
    playbackHistorySchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    insertPlayback,
    getRecentPlayback,
  )
where

--------------------------------------------------------------------------------

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Rel8 hiding (Insert)
import Rel8 qualified

--------------------------------------------------------------------------------
-- Table Definition

-- | The @playback_history@ table definition using rel8's higher-kinded data pattern.
data PlaybackEntry f = PlaybackEntry
  { phId :: Column f Int64,
    phTitle :: Column f Text,
    phArtist :: Column f (Maybe Text),
    phSourceType :: Column f Text,
    phSourceUrl :: Column f Text,
    phStartedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (PlaybackEntry f)

deriving stock instance (f ~ Result) => Eq (PlaybackEntry f)

instance DecodeRow (PlaybackEntry Result)

instance Display (PlaybackEntry Result) where
  displayBuilder entry =
    "PlaybackEntry { id = "
      <> displayBuilder (phId entry)
      <> ", title = "
      <> displayBuilder (phTitle entry)
      <> " }"

-- | Type alias for backwards compatibility.
type Model = PlaybackEntry Result

-- | Table schema connecting the Haskell type to the database table.
playbackHistorySchema :: TableSchema (PlaybackEntry Name)
playbackHistorySchema =
  TableSchema
    { name = "playback_history",
      columns =
        PlaybackEntry
          { phId = "id",
            phTitle = "title",
            phArtist = "artist",
            phSourceType = "source_type",
            phSourceUrl = "source_url",
            phStartedAt = "started_at"
          }
    }

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

--------------------------------------------------------------------------------
-- Queries

-- | Insert a new playback history entry.
insertPlayback :: Insert -> Hasql.Statement () ()
insertPlayback Insert {..} =
  run_ $
    insert
      Rel8.Insert
        { into = playbackHistorySchema,
          rows =
            values
              [ PlaybackEntry
                  { phId = unsafeDefault,
                    phTitle = lit piTitle,
                    phArtist = lit piArtist,
                    phSourceType = lit piSourceType,
                    phSourceUrl = lit piSourceUrl,
                    phStartedAt = lit piStartedAt
                  }
              ],
          onConflict = Abort,
          returning = NoReturning
        }

-- | Get the most recent playback history entries.
--
-- Returns entries ordered by started_at descending (most recent first).
getRecentPlayback :: Int64 -> Hasql.Statement () [Model]
getRecentPlayback lim =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        orderBy (phStartedAt >$< desc) do
          each playbackHistorySchema
