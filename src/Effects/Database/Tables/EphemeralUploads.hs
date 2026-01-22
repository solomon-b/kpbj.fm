{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @ephemeral_uploads@.
--
-- Ephemeral uploads are audio clips used for nighttime playback.
-- Any host can upload ephemeral clips, and they are visible to all hosts.
module Effects.Database.Tables.EphemeralUploads
  ( -- * Id Type
    Id (..),

    -- * Model Type
    Model (..),

    -- * Insert Type
    Insert (..),

    -- * Result Types
    EphemeralUploadWithCreator (..),

    -- * Queries
    getAllEphemeralUploads,
    getEphemeralUploadById,
    insertEphemeralUpload,
    deleteEphemeralUpload,
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

-- | Newtype wrapper for ephemeral upload primary keys.
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

-- | Ephemeral upload record from the database.
data Model = Model
  { eumId :: Id,
    eumTitle :: Text,
    eumAudioFilePath :: Text,
    eumMimeType :: Text,
    eumFileSize :: Int64,
    eumCreatorId :: User.Id,
    eumCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display Model where
  displayBuilder m = displayBuilder m.eumId <> " - " <> displayBuilder m.eumTitle

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new ephemeral uploads.
data Insert = Insert
  { euiTitle :: Text,
    euiAudioFilePath :: Text,
    euiMimeType :: Text,
    euiFileSize :: Int64,
    euiCreatorId :: User.Id
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Result Types

-- | Ephemeral upload with creator display name for list views.
data EphemeralUploadWithCreator = EphemeralUploadWithCreator
  { euwcId :: Id,
    euwcTitle :: Text,
    euwcAudioFilePath :: Text,
    euwcMimeType :: Text,
    euwcFileSize :: Int64,
    euwcCreatorId :: User.Id,
    euwcCreatedAt :: UTCTime,
    euwcCreatorDisplayName :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display EphemeralUploadWithCreator where
  displayBuilder m = displayBuilder m.euwcId <> " - " <> displayBuilder m.euwcTitle

--------------------------------------------------------------------------------
-- Queries

-- | Get all ephemeral uploads with creator display names, ordered by created_at desc.
getAllEphemeralUploads :: Limit -> Offset -> Hasql.Statement () [EphemeralUploadWithCreator]
getAllEphemeralUploads (Limit lim) (Offset off) =
  interp
    True
    [sql|
    SELECT eu.id, eu.title, eu.audio_file_path, eu.mime_type, eu.file_size,
           eu.creator_id, eu.created_at, um.display_name
    FROM ephemeral_uploads eu
    JOIN user_metadata um ON eu.creator_id = um.user_id
    ORDER BY eu.created_at DESC
    LIMIT #{lim}
    OFFSET #{off}
  |]

-- | Get an ephemeral upload by its ID.
getEphemeralUploadById :: Id -> Hasql.Statement () (Maybe Model)
getEphemeralUploadById ephemeralUploadId =
  interp
    False
    [sql|
    SELECT id, title, audio_file_path, mime_type, file_size, creator_id, created_at
    FROM ephemeral_uploads
    WHERE id = #{ephemeralUploadId}
  |]

-- | Insert a new ephemeral upload and return its ID.
insertEphemeralUpload :: Insert -> Hasql.Statement () (Maybe Id)
insertEphemeralUpload Insert {..} =
  interp
    False
    [sql|
    INSERT INTO ephemeral_uploads (title, audio_file_path, mime_type, file_size, creator_id)
    VALUES (#{euiTitle}, #{euiAudioFilePath}, #{euiMimeType}, #{euiFileSize}, #{euiCreatorId})
    RETURNING id
  |]

-- | Delete an ephemeral upload by its ID.
--
-- Returns the ID if successful, Nothing if not found.
deleteEphemeralUpload :: Id -> Hasql.Statement () (Maybe Id)
deleteEphemeralUpload ephemeralUploadId =
  interp
    False
    [sql|
    DELETE FROM ephemeral_uploads
    WHERE id = #{ephemeralUploadId}
    RETURNING id
  |]
