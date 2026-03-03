{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @station_ids@.
--
-- Station IDs are short audio clips used to identify the station during broadcasts.
-- Any host can upload station IDs, and they are visible to all hosts.
--
-- Uses rel8 for type-safe database queries where possible.
module Effects.Database.Tables.StationIds
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    StationId (..),
    stationIdSchema,

    -- * Model (Result alias)
    Model,

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
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for station ID primary keys.
--
-- Provides type safety to prevent mixing up IDs from different tables.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Table Definition

-- | The @station_ids@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data StationId f = StationId
  { simId :: Column f Id,
    simTitle :: Column f Text,
    simAudioFilePath :: Column f Text,
    simMimeType :: Column f Text,
    simFileSize :: Column f Int64,
    simCreatorId :: Column f User.Id,
    simCreatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (StationId f)

deriving stock instance (f ~ Result) => Eq (StationId f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (StationId Result)

-- | Display instance for StationId Result.
instance Display (StationId Result) where
  displayBuilder m = displayBuilder (simId m) <> " - " <> displayBuilder (simTitle m)

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @StationId Result@.
type Model = StationId Result

-- | Table schema connecting the Haskell type to the database table.
stationIdSchema :: TableSchema (StationId Name)
stationIdSchema =
  TableSchema
    { name = "station_ids",
      columns =
        StationId
          { simId = "id",
            simTitle = "title",
            simAudioFilePath = "audio_file_path",
            simMimeType = "mime_type",
            simFileSize = "file_size",
            simCreatorId = "creator_id",
            simCreatedAt = "created_at"
          }
    }

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
getStationIdById stationIdId = fmap listToMaybe $ run $ select do
  row <- each stationIdSchema
  where_ $ simId row ==. lit stationIdId
  pure row

-- | Insert a new station ID and return its ID.
insertStationId :: Insert -> Hasql.Statement () (Maybe Id)
insertStationId Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = stationIdSchema,
            rows =
              values
                [ StationId
                    { simId = nextId "station_ids_id_seq",
                      simTitle = lit siiTitle,
                      simAudioFilePath = lit siiAudioFilePath,
                      simMimeType = lit siiMimeType,
                      simFileSize = lit siiFileSize,
                      simCreatorId = lit siiCreatorId,
                      simCreatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning simId
          }

-- | Delete a station ID by its ID.
--
-- Returns the ID if successful, Nothing if not found.
deleteStationId :: Id -> Hasql.Statement () (Maybe Id)
deleteStationId stationIdId =
  fmap listToMaybe $
    run $
      delete
        Rel8.Delete
          { from = stationIdSchema,
            using = pure (),
            deleteWhere = \_ row -> simId row ==. lit stationIdId,
            returning = Returning simId
          }
