{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @staged_uploads@.
--
-- Staged uploads support YouTube/Bandcamp-style background file uploads.
-- Files are uploaded immediately on selection and stored with a unique token.
-- When the form is submitted, the token is used to claim the upload.
module Effects.Database.Tables.StagedUploads
  ( -- * Id Type
    Id (..),

    -- * Token Type
    Token (..),

    -- * Upload Type Enum
    UploadType (..),

    -- * Status Enum
    Status (..),

    -- * Model Type
    Model (..),

    -- * Insert Type
    Insert (..),

    -- * Queries
    insert,
    getByToken,
    claimUpload,
    getExpiredUploads,
    deleteById,
    deleteByToken,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for staged upload primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Token Type

-- | Unique token for identifying staged uploads.
--
-- Tokens are UUIDs generated when a file is uploaded, returned to the client,
-- and then submitted with the form to claim the upload.
newtype Token = Token {unToken :: Text}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Upload Type Enum

-- | Type of upload determines validation rules and storage location.
--
-- Currently only audio uploads use staged uploads since they are large files
-- that benefit from background uploading. Image uploads use direct form submission.
data UploadType
  = EpisodeAudio
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded, Read)
  deriving anyclass (FromJSON, ToJSON)

instance Display UploadType where
  displayBuilder EpisodeAudio = "episode_audio"

instance DecodeValue UploadType where
  decodeValue = Decoders.enum decodeUploadType

decodeUploadType :: Text -> Maybe UploadType
decodeUploadType = \case
  "episode_audio" -> Just EpisodeAudio
  _ -> Nothing

instance EncodeValue UploadType where
  encodeValue = Encoders.enum $ \case
    EpisodeAudio -> "episode_audio"

instance Servant.FromHttpApiData UploadType where
  parseUrlPiece "episode_audio" = Right EpisodeAudio
  parseUrlPiece invalid = Left $ "Invalid UploadType: " <> invalid

instance Servant.ToHttpApiData UploadType where
  toUrlPiece = Text.pack . show

--------------------------------------------------------------------------------
-- Status Enum

-- | Upload claim status.
data Status
  = Pending -- ^ Upload is awaiting claim by form submission
  | Claimed -- ^ Upload has been claimed and associated with an entity
  | Expired -- ^ Upload has expired and is a candidate for cleanup
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded, Read)
  deriving anyclass (FromJSON, ToJSON)

instance Display Status where
  displayBuilder Pending = "pending"
  displayBuilder Claimed = "claimed"
  displayBuilder Expired = "expired"

instance DecodeValue Status where
  decodeValue = Decoders.enum decodeStatus

decodeStatus :: Text -> Maybe Status
decodeStatus = \case
  "pending" -> Just Pending
  "claimed" -> Just Claimed
  "expired" -> Just Expired
  _ -> Nothing

instance EncodeValue Status where
  encodeValue = Encoders.enum $ \case
    Pending -> "pending"
    Claimed -> "claimed"
    Expired -> "expired"

--------------------------------------------------------------------------------
-- Model Type

-- | Staged upload record from the database.
data Model = Model
  { id :: Id,
    token :: Token,
    userId :: User.Id,
    originalName :: Text,
    storagePath :: Text,
    mimeType :: Text,
    fileSize :: Int64,
    uploadType :: UploadType,
    status :: Status,
    createdAt :: UTCTime,
    claimedAt :: Maybe UTCTime,
    expiresAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display Model where
  displayBuilder m = displayBuilder (token m)

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert data for creating a new staged upload.
data Insert = Insert
  { siToken :: Token,
    siUserId :: User.Id,
    siOriginalName :: Text,
    siStoragePath :: Text,
    siMimeType :: Text,
    siFileSize :: Int64,
    siUploadType :: UploadType
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Insert a new staged upload and return the ID.
insert :: Insert -> Hasql.Statement () (Maybe Id)
insert Insert {..} =
  interp
    False
    [sql|
    INSERT INTO staged_uploads (token, user_id, original_name, storage_path, mime_type, file_size, upload_type)
    VALUES (#{siToken}, #{siUserId}, #{siOriginalName}, #{siStoragePath}, #{siMimeType}, #{siFileSize}, #{siUploadType})
    RETURNING id
  |]

-- | Get a staged upload by its token.
--
-- Returns Nothing if the token doesn't exist.
getByToken :: Token -> Hasql.Statement () (Maybe Model)
getByToken token =
  interp
    False
    [sql|
    SELECT id, token, user_id, original_name, storage_path, mime_type, file_size,
           upload_type, status, created_at, claimed_at, expires_at
    FROM staged_uploads
    WHERE token = #{token}
  |]

-- | Claim an upload by token, verifying user ownership.
--
-- Sets status to 'claimed' and records the claim timestamp.
-- Returns the model if successful, Nothing if:
-- - Token doesn't exist
-- - Token belongs to a different user
-- - Upload is not in 'pending' status
-- - Upload has expired
claimUpload :: Token -> User.Id -> Hasql.Statement () (Maybe Model)
claimUpload token userId =
  interp
    False
    [sql|
    UPDATE staged_uploads
    SET status = 'claimed', claimed_at = NOW()
    WHERE token = #{token}
      AND user_id = #{userId}
      AND status = 'pending'
      AND expires_at > NOW()
    RETURNING id, token, user_id, original_name, storage_path, mime_type, file_size,
              upload_type, status, created_at, claimed_at, expires_at
  |]

-- | Get all expired pending uploads for cleanup.
--
-- Returns uploads where status = 'pending' and expires_at < NOW().
getExpiredUploads :: Hasql.Statement () [Model]
getExpiredUploads =
  interp
    False
    [sql|
    SELECT id, token, user_id, original_name, storage_path, mime_type, file_size,
           upload_type, status, created_at, claimed_at, expires_at
    FROM staged_uploads
    WHERE status = 'pending' AND expires_at < NOW()
  |]

-- | Delete a staged upload by ID.
--
-- Returns the ID if successful, Nothing if not found.
deleteById :: Id -> Hasql.Statement () (Maybe Id)
deleteById uploadId =
  interp
    False
    [sql|
    DELETE FROM staged_uploads
    WHERE id = #{uploadId}
    RETURNING id
  |]

-- | Delete a staged upload by token.
--
-- Returns the ID if successful, Nothing if not found.
deleteByToken :: Token -> Hasql.Statement () (Maybe Id)
deleteByToken token =
  interp
    False
    [sql|
    DELETE FROM staged_uploads
    WHERE token = #{token}
    RETURNING id
  |]
