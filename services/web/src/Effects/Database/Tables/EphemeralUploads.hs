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
    getRandomEphemeralUpload,
    insertEphemeralUpload,
    updateEphemeralUpload,
    deleteEphemeralUpload,
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
    eumAudioFilePath :: Column f Text,
    eumMimeType :: Column f Text,
    eumFileSize :: Column f Int64,
    eumCreatorId :: Column f User.Id,
    eumCreatedAt :: Column f UTCTime
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
            eumAudioFilePath = "audio_file_path",
            eumMimeType = "mime_type",
            eumFileSize = "file_size",
            eumCreatorId = "creator_id",
            eumCreatedAt = "created_at"
          }
    }

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
getEphemeralUploadById ephemeralUploadId = fmap listToMaybe $ run $ select do
  row <- each ephemeralUploadSchema
  where_ $ eumId row ==. lit ephemeralUploadId
  pure row

-- | Get a random ephemeral upload for fallback playback.
--
-- Returns a randomly selected ephemeral upload, or Nothing if the table is empty.
-- Used by Liquidsoap for fallback audio when no show is scheduled.
getRandomEphemeralUpload :: Hasql.Statement () (Maybe Model)
getRandomEphemeralUpload =
  interp
    False
    [sql|
    SELECT id, title, audio_file_path, mime_type, file_size, creator_id, created_at
    FROM ephemeral_uploads
    ORDER BY RANDOM()
    LIMIT 1
  |]

-- | Insert a new ephemeral upload and return its ID.
insertEphemeralUpload :: Insert -> Hasql.Statement () (Maybe Id)
insertEphemeralUpload Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = ephemeralUploadSchema,
            rows =
              values
                [ EphemeralUpload
                    { eumId = nextId "ephemeral_uploads_id_seq",
                      eumTitle = lit euiTitle,
                      eumAudioFilePath = lit euiAudioFilePath,
                      eumMimeType = lit euiMimeType,
                      eumFileSize = lit euiFileSize,
                      eumCreatorId = lit euiCreatorId,
                      eumCreatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning eumId
          }

-- | Update an ephemeral upload.
--
-- Updates all mutable fields. Pass existing values for fields you don't want to change.
-- Returns the updated model if successful, Nothing if not found.
updateEphemeralUpload :: Id -> Text -> Text -> Text -> Int64 -> Hasql.Statement () (Maybe Model)
updateEphemeralUpload ephemeralUploadId newTitle newAudioFilePath newMimeType newFileSize =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = ephemeralUploadSchema,
            from = pure (),
            set = \_ row ->
              row
                { eumTitle = lit newTitle,
                  eumAudioFilePath = lit newAudioFilePath,
                  eumMimeType = lit newMimeType,
                  eumFileSize = lit newFileSize
                },
            updateWhere = \_ row -> eumId row ==. lit ephemeralUploadId,
            returning = Returning Prelude.id
          }

-- | Delete an ephemeral upload by its ID.
--
-- Returns the ID if successful, Nothing if not found.
deleteEphemeralUpload :: Id -> Hasql.Statement () (Maybe Id)
deleteEphemeralUpload ephemeralUploadId =
  fmap listToMaybe $
    run $
      delete
        Rel8.Delete
          { from = ephemeralUploadSchema,
            using = pure (),
            deleteWhere = \_ row -> eumId row ==. lit ephemeralUploadId,
            returning = Returning eumId
          }
