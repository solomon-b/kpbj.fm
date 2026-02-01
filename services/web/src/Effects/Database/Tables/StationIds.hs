{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @station_ids@.
--
-- Station IDs are short audio clips used to identify the station during broadcasts.
-- Any host can upload station IDs, and they are visible to all hosts.
module Effects.Database.Tables.StationIds
  ( -- * Id Type
    Id (..),

    -- * Model Type
    Model (..),

    -- * Insert Type
    Insert (..),

    -- * Result Types
    StationIdWithCreator (..),

    -- * Queries
    getAllStationIds,
    getStationIdById,
    insertStationId,
    deleteStationId,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for station ID primary keys.
--
-- Provides type safety to prevent mixing up IDs from different tables.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Model Type

-- | Station ID record from the database.
data Model = Model
  { simId :: Id,
    simTitle :: Text,
    simAudioFilePath :: Text,
    simMimeType :: Text,
    simFileSize :: Int64,
    simCreatorId :: User.Id,
    simCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display Model where
  displayBuilder m = displayBuilder m.simId <> " - " <> displayBuilder m.simTitle

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new station IDs.
data Insert = Insert
  { siiTitle :: Text,
    siiAudioFilePath :: Text,
    siiMimeType :: Text,
    siiFileSize :: Int64,
    siiCreatorId :: User.Id
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Result Types

-- | Station ID with creator display name for list views.
data StationIdWithCreator = StationIdWithCreator
  { siwcId :: Id,
    siwcTitle :: Text,
    siwcAudioFilePath :: Text,
    siwcMimeType :: Text,
    siwcFileSize :: Int64,
    siwcCreatorId :: User.Id,
    siwcCreatedAt :: UTCTime,
    siwcCreatorDisplayName :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display StationIdWithCreator where
  displayBuilder m = displayBuilder m.siwcId <> " - " <> displayBuilder m.siwcTitle

--------------------------------------------------------------------------------
-- Queries

-- | Get all station IDs with creator display names, ordered by created_at desc.
getAllStationIds :: Limit -> Offset -> Hasql.Statement () [StationIdWithCreator]
getAllStationIds (Limit lim) (Offset off) =
  interp
    True
    [sql|
    SELECT si.id, si.title, si.audio_file_path, si.mime_type, si.file_size,
           si.creator_id, si.created_at, um.display_name
    FROM station_ids si
    JOIN user_metadata um ON si.creator_id = um.user_id
    ORDER BY si.created_at DESC
    LIMIT #{lim}
    OFFSET #{off}
  |]

-- | Get a station ID by its ID.
getStationIdById :: Id -> Hasql.Statement () (Maybe Model)
getStationIdById stationIdId =
  interp
    False
    [sql|
    SELECT id, title, audio_file_path, mime_type, file_size, creator_id, created_at
    FROM station_ids
    WHERE id = #{stationIdId}
  |]

-- | Insert a new station ID and return its ID.
insertStationId :: Insert -> Hasql.Statement () (Maybe Id)
insertStationId Insert {..} =
  interp
    False
    [sql|
    INSERT INTO station_ids (title, audio_file_path, mime_type, file_size, creator_id)
    VALUES (#{siiTitle}, #{siiAudioFilePath}, #{siiMimeType}, #{siiFileSize}, #{siiCreatorId})
    RETURNING id
  |]

-- | Delete a station ID by its ID.
--
-- Returns the ID if successful, Nothing if not found.
deleteStationId :: Id -> Hasql.Statement () (Maybe Id)
deleteStationId stationIdId =
  interp
    False
    [sql|
    DELETE FROM station_ids
    WHERE id = #{stationIdId}
    RETURNING id
  |]
