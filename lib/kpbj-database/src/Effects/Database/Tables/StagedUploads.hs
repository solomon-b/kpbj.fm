{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

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

    -- * Table Definition
    StagedUpload (..),
    stagedUploadSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    insert,
    getByToken,
    claimUpload,
    deleteById,
    getExpiredUploads,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
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
import OrphanInstances.Rel8 ()
import Rel8 hiding (Enum, Insert, insert)
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for staged upload primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq)
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
  deriving newtype (Show, Eq, Ord, DBType, DBEq)
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
  | StationIdAudio
  | EphemeralAudio
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded, Read)
  deriving anyclass (FromJSON, ToJSON)

instance DBType UploadType where
  typeInformation =
    parseTypeInformation
      ( \case
          "episode_audio" -> Right EpisodeAudio
          "station_id_audio" -> Right StationIdAudio
          "ephemeral_audio" -> Right EphemeralAudio
          other -> Left $ "Invalid UploadType: " <> Text.unpack other
      )
      ( \case
          EpisodeAudio -> "episode_audio"
          StationIdAudio -> "station_id_audio"
          EphemeralAudio -> "ephemeral_audio"
      )
      typeInformation

instance DBEq UploadType

instance Display UploadType where
  displayBuilder EpisodeAudio = "episode_audio"
  displayBuilder StationIdAudio = "station_id_audio"
  displayBuilder EphemeralAudio = "ephemeral_audio"

instance DecodeValue UploadType where
  decodeValue = Decoders.enum decodeUploadType

decodeUploadType :: Text -> Maybe UploadType
decodeUploadType = \case
  "episode_audio" -> Just EpisodeAudio
  "station_id_audio" -> Just StationIdAudio
  "ephemeral_audio" -> Just EphemeralAudio
  _ -> Nothing

instance EncodeValue UploadType where
  encodeValue = Encoders.enum $ \case
    EpisodeAudio -> "episode_audio"
    StationIdAudio -> "station_id_audio"
    EphemeralAudio -> "ephemeral_audio"

instance Servant.FromHttpApiData UploadType where
  parseUrlPiece "episode_audio" = Right EpisodeAudio
  parseUrlPiece "station_id_audio" = Right StationIdAudio
  parseUrlPiece "ephemeral_audio" = Right EphemeralAudio
  parseUrlPiece invalid = Left $ "Invalid UploadType: " <> invalid

instance Servant.ToHttpApiData UploadType where
  toUrlPiece = Text.pack . show

--------------------------------------------------------------------------------
-- Status Enum

-- | Upload claim status.
data Status
  = -- | Upload is awaiting claim by form submission
    Pending
  | -- | Upload has been claimed and associated with an entity
    Claimed
  | -- | Upload has expired and is a candidate for cleanup
    Expired
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded, Read)
  deriving anyclass (FromJSON, ToJSON)

instance DBType Status where
  typeInformation =
    parseTypeInformation
      ( \case
          "pending" -> Right Pending
          "claimed" -> Right Claimed
          "expired" -> Right Expired
          other -> Left $ "Invalid Status: " <> Text.unpack other
      )
      ( \case
          Pending -> "pending"
          Claimed -> "claimed"
          Expired -> "expired"
      )
      typeInformation

instance DBEq Status

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
-- Table Definition

-- | The @staged_uploads@ table definition using rel8's higher-kinded data pattern.
data StagedUpload f = StagedUpload
  { id :: Column f Id,
    token :: Column f Token,
    userId :: Column f User.Id,
    originalName :: Column f Text,
    storagePath :: Column f Text,
    mimeType :: Column f Text,
    fileSize :: Column f Int64,
    uploadType :: Column f UploadType,
    status :: Column f Status,
    createdAt :: Column f UTCTime,
    claimedAt :: Column f (Maybe UTCTime),
    expiresAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (StagedUpload f)

deriving stock instance (f ~ Result) => Eq (StagedUpload f)

instance DecodeRow (StagedUpload Result)

instance Display (StagedUpload Result) where
  displayBuilder m = displayBuilder ((.token) m)

-- | Type alias for backwards compatibility.
type Model = StagedUpload Result

-- | Table schema connecting the Haskell type to the database table.
stagedUploadSchema :: TableSchema (StagedUpload Name)
stagedUploadSchema =
  TableSchema
    { name = "staged_uploads",
      columns =
        StagedUpload
          { id = "id",
            token = "token",
            userId = "user_id",
            originalName = "original_name",
            storagePath = "storage_path",
            mimeType = "mime_type",
            fileSize = "file_size",
            uploadType = "upload_type",
            status = "status",
            createdAt = "created_at",
            claimedAt = "claimed_at",
            expiresAt = "expires_at"
          }
    }

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
--
-- Uses raw SQL because expires_at relies on the DB default (NOW() + INTERVAL '1 hour').
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
getByToken tokenVal = fmap listToMaybe $ run $ select do
  row <- each stagedUploadSchema
  where_ $ (.token) row ==. lit tokenVal
  pure row

-- | Claim an upload by token, verifying user ownership.
--
-- Sets status to 'claimed' and records the claim timestamp.
-- Returns the model if successful, Nothing if:
-- - Token doesn't exist
-- - Token belongs to a different user
-- - Upload is not in 'pending' status
-- - Upload has expired
claimUpload :: Token -> User.Id -> Hasql.Statement () (Maybe Model)
claimUpload tokenVal userIdVal =
  fmap listToMaybe $
    run $
      update
        Update
          { target = stagedUploadSchema,
            from = pure (),
            set = \_ row ->
              row
                { status = lit Claimed,
                  claimedAt = nullify now
                },
            updateWhere = \_ row ->
              (.token) row ==. lit tokenVal
                &&. (.userId) row ==. lit userIdVal
                &&. (.status) row ==. lit Pending
                &&. (.expiresAt) row >. now,
            returning = Returning Prelude.id
          }

-- | Delete a staged upload by ID.
--
-- Returns the ID if successful, Nothing if not found.
deleteById :: Id -> Hasql.Statement () (Maybe Id)
deleteById uploadId =
  fmap listToMaybe $
    run $
      delete
        Delete
          { from = stagedUploadSchema,
            using = pure (),
            deleteWhere = \_ row -> (.id) row ==. lit uploadId,
            returning = Returning (.id)
          }

-- | Get all expired uploads that are still pending.
--
-- Returns uploads with status 'pending' and expires_at < NOW().
-- These are candidates for cleanup.
getExpiredUploads :: Hasql.Statement () [Model]
getExpiredUploads =
  run $ select do
    row <- each stagedUploadSchema
    where_ $ (.status) row ==. lit Pending
    where_ $ (.expiresAt) row <. now
    pure row
