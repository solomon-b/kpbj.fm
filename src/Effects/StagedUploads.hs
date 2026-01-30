-- | Shared operations for staged file uploads.
--
-- This module provides:
-- - Cryptographically secure token generation
-- - Upload claiming with authorization verification
-- - File relocation on claim (move from staging to final location)
-- - File cleanup utilities
module Effects.StagedUploads
  ( -- * Token Generation
    generateSecureToken,

    -- * Upload Claiming
    claimStagedUpload,
    claimAndRelocateUpload,
    ClaimError (..),
    claimErrorToText,
  )
where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import App.Monad (AppM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Domain.Types.FileStorage (BucketType (..), DateHierarchy (..), ResourceType (..), dateHierarchyFromTime)
import Domain.Types.StorageBackend (StorageBackend (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.Storage.Local qualified as Local
import Effects.Storage.S3 qualified as S3
import Log qualified
import System.FilePath (takeExtension)

--------------------------------------------------------------------------------
-- Token Generation

-- | Generate a cryptographically secure token for staged uploads.
--
-- Uses UUID v4 (random) which is generated from a cryptographically secure
-- random number generator. The token is formatted as a 32-character hex string
-- (UUID without hyphens) for URL-safety and compact storage.
generateSecureToken :: (MonadIO m) => m StagedUploads.Token
generateSecureToken = liftIO $ do
  uuid <- UUID.V4.nextRandom
  -- Remove hyphens from UUID for cleaner tokens: "550e8400-e29b-..." -> "550e8400e29b..."
  let tokenText = Text.filter (/= '-') $ UUID.toText uuid
  pure $ StagedUploads.Token tokenText

--------------------------------------------------------------------------------
-- Upload Claiming

-- | Errors that can occur when claiming a staged upload.
data ClaimError
  = -- | Database query failed
    ClaimDbError Text
  | -- | Token not found, expired, or belongs to different user
    ClaimNotFound
  | -- | Upload type doesn't match expected type
    ClaimTypeMismatch StagedUploads.UploadType StagedUploads.UploadType
  deriving stock (Show, Eq)

-- | Claim a staged upload by token, returning the storage path.
--
-- Verifies:
-- - Token exists and is in 'pending' status
-- - Upload belongs to the specified user
-- - Upload type matches the expected type
--
-- On success, marks the upload as 'claimed' and returns the storage path.
claimStagedUpload ::
  User.Id ->
  Text ->
  StagedUploads.UploadType ->
  AppM (Either ClaimError Text)
claimStagedUpload userId tokenText expectedType = do
  let token = StagedUploads.Token tokenText
  result <- execQuery (StagedUploads.claimUpload token userId)
  case result of
    Left err -> do
      Log.logInfo "Failed to claim staged upload" (Text.pack $ show err)
      pure $ Left $ ClaimDbError "Failed to claim uploaded file"
    Right Nothing -> do
      Log.logInfo "Staged upload not found or not authorized" tokenText
      pure $ Left ClaimNotFound
    Right (Just stagedUpload) -> do
      -- Verify the upload type matches
      let actualType = StagedUploads.uploadType stagedUpload
      if actualType /= expectedType
        then do
          Log.logInfo "Staged upload type mismatch" (Text.pack $ show (actualType, expectedType))
          pure $ Left $ ClaimTypeMismatch actualType expectedType
        else do
          Log.logInfo "Claimed staged upload" (StagedUploads.storagePath stagedUpload)
          pure $ Right $ StagedUploads.storagePath stagedUpload

-- | Convert a ClaimError to user-friendly text.
claimErrorToText :: ClaimError -> Text
claimErrorToText = \case
  ClaimDbError msg -> msg
  ClaimNotFound -> "Uploaded file not found or expired"
  ClaimTypeMismatch _ _ -> "Uploaded file type mismatch"

-- | Claim a staged upload and relocate it to the final storage location.
--
-- This function:
-- 1. Claims the staged upload (verifies ownership, marks as claimed)
-- 2. Moves the file from the staging area to the final location with the
--    correct date hierarchy based on the provided air date
-- 3. Returns the new storage path for the database
--
-- Use this instead of 'claimStagedUpload' when you need the file in its
-- final archive location (e.g., episode audio files organized by air date).
claimAndRelocateUpload ::
  -- | User ID (for authorization)
  User.Id ->
  -- | Upload token
  Text ->
  -- | Expected upload type
  StagedUploads.UploadType ->
  -- | Destination bucket type
  BucketType ->
  -- | Destination resource type
  ResourceType ->
  -- | Air date (used for date hierarchy in final path)
  UTCTime ->
  -- | Filename prefix (e.g., show slug)
  Text ->
  AppM (Either Text Text)
claimAndRelocateUpload userId tokenText expectedType destBucket destResource airDate filenamePrefix = do
  -- First claim the upload
  claimResult <- claimStagedUpload userId tokenText expectedType
  case claimResult of
    Left err -> pure $ Left $ claimErrorToText err
    Right stagingPath -> do
      -- Move to final location
      backend <- asks (Has.getter @StorageBackend)
      mAwsEnv <- asks (Has.getter @(Maybe AWS.Env))

      let dateHier = dateHierarchyFromTime airDate
          -- Extract extension from the staged file
          extension = Text.pack $ drop 1 $ takeExtension $ Text.unpack stagingPath
          -- Format air date as YYYY-MM-DD for filename
          dateStr = dateYear dateHier <> "-" <> dateMonth dateHier <> "-" <> dateDay dateHier

      -- Generate a unique filename for the final location using UUID
      uuid <- liftIO UUID.V4.nextRandom
      let uuidText = Text.filter (/= '-') $ UUID.toText uuid
          destFilename = filenamePrefix <> "_" <> dateStr <> "_" <> uuidText <> "." <> extension

      moveResult <- moveFile backend mAwsEnv stagingPath destBucket destResource dateHier destFilename

      case moveResult of
        Left err -> do
          Log.logInfo "Failed to relocate staged upload" err
          -- Return the staging path as fallback (file is still accessible)
          pure $ Right stagingPath
        Right newPath -> do
          Log.logInfo "Relocated staged upload to final location" newPath
          pure $ Right newPath

--------------------------------------------------------------------------------
-- Internal Helpers

-- | Move a file using the configured storage backend.
moveFile ::
  (MonadIO m) =>
  StorageBackend ->
  Maybe AWS.Env ->
  -- | Source path (staging location)
  Text ->
  -- | Destination bucket type
  BucketType ->
  -- | Destination resource type
  ResourceType ->
  -- | Destination date hierarchy
  DateHierarchy ->
  -- | Destination filename
  Text ->
  m (Either Text Text)
moveFile backend mAwsEnv sourcePath destBucket destResource destDateHier destFilename =
  case backend of
    LocalStorage config ->
      Local.moveFileLocal config sourcePath destBucket destResource destDateHier destFilename
    S3Storage config -> case mAwsEnv of
      Nothing -> pure $ Left "S3 storage requires AWS environment"
      Just awsEnv ->
        S3.moveFileS3 awsEnv config sourcePath destBucket destResource destDateHier destFilename
