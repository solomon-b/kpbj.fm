{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @stream_settings@.
--
-- Singleton table storing the Icecast stream URL and metadata URL used by
-- the web player. Only one row can exist (enforced by CHECK constraint).
module Effects.Database.Tables.StreamSettings
  ( -- * Model
    Model (..),

    -- * Update Type
    Update (..),

    -- * Queries
    getStreamSettings,
    updateStreamSettings,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()

--------------------------------------------------------------------------------
-- Model

-- | Stream settings model representing the single row from @stream_settings@.
data Model = Model
  { ssStreamUrl :: Text,
    ssMetadataUrl :: Text,
    ssUpdatedAt :: UTCTime,
    ssUpdatedBy :: Maybe User.Id
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

--------------------------------------------------------------------------------
-- Update Type

-- | Update type for modifying stream settings.
data Update = Update
  { ssuStreamUrl :: Text,
    ssuMetadataUrl :: Text
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Get the stream settings.
--
-- Returns the singleton row from the stream_settings table.
getStreamSettings :: Hasql.Statement () (Maybe Model)
getStreamSettings =
  interp
    False
    [sql|
    SELECT stream_url, metadata_url, updated_at, updated_by
    FROM stream_settings
    WHERE id = 1
  |]

-- | Update stream settings and return the updated model.
--
-- The user ID is recorded for audit purposes.
updateStreamSettings :: User.Id -> Update -> Hasql.Statement () (Maybe Model)
updateStreamSettings userId Update {..} =
  interp
    False
    [sql|
    UPDATE stream_settings
    SET stream_url = #{ssuStreamUrl},
        metadata_url = #{ssuMetadataUrl},
        updated_at = NOW(),
        updated_by = #{userId}
    WHERE id = 1
    RETURNING stream_url, metadata_url, updated_at, updated_by
  |]
