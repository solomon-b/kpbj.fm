{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @ephemeral_uploads@.
--
-- Ephemeral uploads are audio clips used for nighttime playback.
-- Any host can upload ephemeral clips, and they are visible to all hosts.
--
-- Uses rel8 for type-safe database queries where possible.
module Effects.Database.Tables.EphemeralUploads
  ( -- * Id Type
    Id (..),

    -- * Flag Reason
    FlagReason (..),
    flagReasonToText,
    parseFlagReason,

    -- * Table Definition
    EphemeralUpload (..),
    ephemeralUploadSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Result Types
    EphemeralUploadWithCreator (..),

    -- * Queries
    getAllEphemeralUploads,
    getEphemeralUploadById,
    getEphemeralUploadWithCreatorById,
    getRandomEphemeralUpload,
    insertEphemeralUpload,
    updateEphemeralUpload,
    deleteEphemeralUpload,
    flagEphemeralUpload,
    unflagEphemeralUpload,
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
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Enum, Insert)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for ephemeral upload primary keys.
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
-- Flag Reason

-- | Predefined reasons for flagging an ephemeral upload.
data FlagReason
  = InappropriateContent
  | PoorAudioQuality
  | CopyrightConcern
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)

instance DBType FlagReason where
  typeInformation =
    parseTypeInformation
      ( \case
          "inappropriate_content" -> Right InappropriateContent
          "poor_audio_quality" -> Right PoorAudioQuality
          "copyright_concern" -> Right CopyrightConcern
          other -> Left $ "Invalid FlagReason: " <> Text.unpack other
      )
      ( \case
          InappropriateContent -> "inappropriate_content"
          PoorAudioQuality -> "poor_audio_quality"
          CopyrightConcern -> "copyright_concern"
      )
      typeInformation

instance DBEq FlagReason

instance DecodeValue FlagReason where
  decodeValue = Decoders.enum $ \case
    "inappropriate_content" -> Just InappropriateContent
    "poor_audio_quality" -> Just PoorAudioQuality
    "copyright_concern" -> Just CopyrightConcern
    _ -> Nothing

instance EncodeValue FlagReason where
  encodeValue = Encoders.enum $ \case
    InappropriateContent -> "inappropriate_content"
    PoorAudioQuality -> "poor_audio_quality"
    CopyrightConcern -> "copyright_concern"

instance Display FlagReason where
  displayBuilder = \case
    InappropriateContent -> "Inappropriate content"
    PoorAudioQuality -> "Poor audio quality"
    CopyrightConcern -> "Copyright concern"

-- | Convert a flag reason to its human-readable label.
flagReasonToText :: FlagReason -> Text
flagReasonToText = \case
  InappropriateContent -> "Inappropriate content"
  PoorAudioQuality -> "Poor audio quality"
  CopyrightConcern -> "Copyright concern"

-- | Parse a human-readable label into a flag reason.
parseFlagReason :: Text -> Maybe FlagReason
parseFlagReason = \case
  "Inappropriate content" -> Just InappropriateContent
  "Poor audio quality" -> Just PoorAudioQuality
  "Copyright concern" -> Just CopyrightConcern
  _ -> Nothing

--------------------------------------------------------------------------------
-- Table Definition

-- | The @ephemeral_uploads@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data EphemeralUpload f = EphemeralUpload
  { eumId :: Column f Id,
    eumTitle :: Column f Text,
    eumDescription :: Column f Text,
    eumAudioFilePath :: Column f Text,
    eumMimeType :: Column f Text,
    eumFileSize :: Column f Int64,
    eumCreatorId :: Column f User.Id,
    eumCreatedAt :: Column f UTCTime,
    eumFlaggedAt :: Column f (Maybe UTCTime),
    eumFlaggedBy :: Column f (Maybe User.Id),
    eumFlagReason :: Column f (Maybe FlagReason)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (EphemeralUpload f)

deriving stock instance (f ~ Result) => Eq (EphemeralUpload f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (EphemeralUpload Result)

-- | Display instance for EphemeralUpload Result.
instance Display (EphemeralUpload Result) where
  displayBuilder m = displayBuilder (eumId m) <> " - " <> displayBuilder (eumTitle m)

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @EphemeralUpload Result@.
type Model = EphemeralUpload Result

-- | Table schema connecting the Haskell type to the database table.
ephemeralUploadSchema :: TableSchema (EphemeralUpload Name)
ephemeralUploadSchema =
  TableSchema
    { name = "ephemeral_uploads",
      columns =
        EphemeralUpload
          { eumId = "id",
            eumTitle = "title",
            eumDescription = "description",
            eumAudioFilePath = "audio_file_path",
            eumMimeType = "mime_type",
            eumFileSize = "file_size",
            eumCreatorId = "creator_id",
            eumCreatedAt = "created_at",
            eumFlaggedAt = "flagged_at",
            eumFlaggedBy = "flagged_by",
            eumFlagReason = "flag_reason"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new ephemeral uploads.
data Insert = Insert
  { euiTitle :: Text,
    euiDescription :: Text,
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
    euwcDescription :: Text,
    euwcAudioFilePath :: Text,
    euwcMimeType :: Text,
    euwcFileSize :: Int64,
    euwcCreatorId :: User.Id,
    euwcCreatedAt :: UTCTime,
    euwcCreatorDisplayName :: Text,
    euwcFlaggedAt :: Maybe UTCTime,
    euwcFlaggedBy :: Maybe User.Id,
    euwcFlagReason :: Maybe FlagReason
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display EphemeralUploadWithCreator where
  displayBuilder m = displayBuilder m.euwcId <> " - " <> displayBuilder m.euwcTitle

--------------------------------------------------------------------------------
-- Queries

-- | Get all ephemeral uploads with creator display names, ordered by created_at desc.
--
-- When @includeFlagged@ is 'False', only unflagged uploads are returned.
-- When @includeFlagged@ is 'True', all uploads are returned with flagged sorted to top.
getAllEphemeralUploads :: Bool -> Limit -> Offset -> Hasql.Statement () [EphemeralUploadWithCreator]
getAllEphemeralUploads includeFlagged (Limit lim) (Offset off)
  | includeFlagged =
      interp
        True
        [sql|
        SELECT eu.id, eu.title, eu.description, eu.audio_file_path, eu.mime_type, eu.file_size,
               eu.creator_id, eu.created_at, um.display_name,
               eu.flagged_at, eu.flagged_by, eu.flag_reason
        FROM ephemeral_uploads eu
        JOIN user_metadata um ON eu.creator_id = um.user_id
        ORDER BY (eu.flagged_at IS NOT NULL) DESC, eu.created_at DESC
        LIMIT #{lim}
        OFFSET #{off}
      |]
  | otherwise =
      interp
        True
        [sql|
        SELECT eu.id, eu.title, eu.description, eu.audio_file_path, eu.mime_type, eu.file_size,
               eu.creator_id, eu.created_at, um.display_name,
               eu.flagged_at, eu.flagged_by, eu.flag_reason
        FROM ephemeral_uploads eu
        JOIN user_metadata um ON eu.creator_id = um.user_id
        WHERE eu.flagged_at IS NULL
        ORDER BY eu.created_at DESC
        LIMIT #{lim}
        OFFSET #{off}
      |]

-- | Get an ephemeral upload by its ID.
getEphemeralUploadById :: Id -> Hasql.Statement () (Maybe Model)
getEphemeralUploadById ephemeralUploadId = fmap listToMaybe $ run $ select do
  row <- each ephemeralUploadSchema
  where_ $ eumId row ==. lit ephemeralUploadId
  pure row

-- | Get a random unflagged ephemeral upload for fallback playback.
--
-- Returns a randomly selected ephemeral upload, or Nothing if no unflagged uploads exist.
-- Used by Liquidsoap for fallback audio when no show is scheduled.
-- Flagged uploads are excluded from fallback playback.
getRandomEphemeralUpload :: Hasql.Statement () (Maybe Model)
getRandomEphemeralUpload =
  interp
    False
    [sql|
    SELECT id, title, description, audio_file_path, mime_type, file_size, creator_id, created_at,
           flagged_at, flagged_by, flag_reason
    FROM ephemeral_uploads
    WHERE flagged_at IS NULL
    ORDER BY RANDOM()
    LIMIT 1
  |]

-- | Insert a new ephemeral upload and return its ID.
insertEphemeralUpload :: Insert -> Hasql.Statement () (Maybe Id)
insertEphemeralUpload Insert {..} =
  listToMaybe
    <$> interp
      False
      [sql|
      INSERT INTO ephemeral_uploads(title, description, audio_file_path, mime_type, file_size, creator_id, created_at)
      VALUES (#{euiTitle}, #{euiDescription}, #{euiAudioFilePath}, #{euiMimeType}, #{euiFileSize}, #{euiCreatorId}, NOW())
      RETURNING id
    |]

-- | Update an ephemeral upload.
--
-- Updates all mutable fields. Pass existing values for fields you don't want to change.
-- Returns the updated model if successful, Nothing if not found.
updateEphemeralUpload :: Id -> Text -> Text -> Text -> Text -> Int64 -> Hasql.Statement () (Maybe Model)
updateEphemeralUpload (Id eid) newTitle newDescription newAudioFilePath newMimeType newFileSize =
  listToMaybe
    <$> interp
      False
      [sql|
      UPDATE ephemeral_uploads
      SET title = #{newTitle}, description = #{newDescription},
          audio_file_path = #{newAudioFilePath}, mime_type = #{newMimeType}, file_size = #{newFileSize}
      WHERE id = #{eid}
      RETURNING id, title, description, audio_file_path, mime_type, file_size, creator_id, created_at,
                flagged_at, flagged_by, flag_reason
    |]

-- | Delete an ephemeral upload by its ID.
--
-- Returns the ID if successful, Nothing if not found.
deleteEphemeralUpload :: Id -> Hasql.Statement () (Maybe Id)
deleteEphemeralUpload ephemeralUploadId =
  fmap listToMaybe $
    run $
      delete
        Delete
          { from = ephemeralUploadSchema,
            using = pure (),
            deleteWhere = \_ row -> eumId row ==. lit ephemeralUploadId,
            returning = Returning eumId
          }

-- | Flag an ephemeral upload with a reason.
--
-- Sets flagged_at, flagged_by, and flag_reason on the upload.
-- Returns the updated model if successful, Nothing if not found.
flagEphemeralUpload :: Id -> User.Id -> FlagReason -> Hasql.Statement () (Maybe Model)
flagEphemeralUpload ephemeralUploadId flaggerId reason =
  listToMaybe
    <$> interp
      False
      [sql|
      UPDATE ephemeral_uploads
      SET flagged_at = NOW(), flagged_by = #{flaggerId}, flag_reason = #{reason}::flag_reason
      WHERE id = #{ephemeralUploadId}
      RETURNING id, title, description, audio_file_path, mime_type, file_size, creator_id, created_at,
                flagged_at, flagged_by, flag_reason
    |]

-- | Remove the flag from an ephemeral upload.
--
-- Clears flagged_at and flagged_by.
-- Returns the updated model if successful, Nothing if not found.
unflagEphemeralUpload :: Id -> Hasql.Statement () (Maybe Model)
unflagEphemeralUpload (Id eid) =
  listToMaybe
    <$> interp
      False
      [sql|
      UPDATE ephemeral_uploads
      SET flagged_at = NULL, flagged_by = NULL, flag_reason = NULL
      WHERE id = #{eid}
      RETURNING id, title, description, audio_file_path, mime_type, file_size, creator_id, created_at,
                flagged_at, flagged_by, flag_reason
    |]

-- | Get an ephemeral upload with creator info by ID.
--
-- Used for re-rendering a single row after flag/unflag operations.
getEphemeralUploadWithCreatorById :: Id -> Hasql.Statement () (Maybe EphemeralUploadWithCreator)
getEphemeralUploadWithCreatorById (Id eid) =
  listToMaybe
    <$> interp
      True
      [sql|
      SELECT eu.id, eu.title, eu.description, eu.audio_file_path, eu.mime_type, eu.file_size,
             eu.creator_id, eu.created_at, um.display_name,
             eu.flagged_at, eu.flagged_by, eu.flag_reason
      FROM ephemeral_uploads eu
      JOIN user_metadata um ON eu.creator_id = um.user_id
      WHERE eu.id = #{eid}
    |]
