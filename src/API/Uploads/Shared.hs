{-# LANGUAGE QuasiQuotes #-}

-- | Shared logic for staged file upload handlers.
--
-- This module provides the common upload processing pipeline used by
-- audio and image upload endpoints. The pipeline:
--
-- 1. Validates the file using MIME type detection
-- 2. Stores the file with a unique filename
-- 3. Creates a staged upload record with a claim token
-- 4. Cleans up on failure to prevent orphaned files
module API.Uploads.Shared
  ( -- * Processing Pipeline
    processStagedUpload,
    ProcessConfig (..),

    -- * File Validation
    FileValidator,

    -- * Extension Mapping
    ExtensionMapper,
  )
where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import App.Monad (AppM)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.FileStorage (BucketType (..))
import Domain.Types.FileUpload (UploadError (..))
import Domain.Types.StorageBackend (StorageBackend (..))
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.StagedUploadCleanup (deleteFile)
import Effects.StagedUploads (generateSecureToken)
import Effects.Storage.Local qualified as Local (storeFileStagingLocal)
import Effects.Storage.S3 qualified as S3 (storeFileStagingS3)
import Log qualified
import System.IO (hClose)
import System.IO.Temp qualified as Temp

--------------------------------------------------------------------------------
-- Types

-- | A function that validates a file and returns its MIME type.
--
-- Takes the temp file path and browser-reported MIME type.
-- Returns either an error message or the validated MIME type.
type FileValidator m = FilePath -> Text -> m (Either Text Text)

-- | A function that maps MIME type to file extension.
type ExtensionMapper = Text -> Text

-- | Configuration for processing a staged upload.
data ProcessConfig = ProcessConfig
  { -- | File validator (audio or image)
    pcValidator :: forall m. (MonadIO m, Log.MonadLog m) => FileValidator m,
    -- | MIME type to extension mapper
    pcExtensionMapper :: ExtensionMapper,
    -- | Upload type for the staged upload record
    pcUploadType :: StagedUploads.UploadType,
    -- | Prefix for temp file names (e.g., "staged-audio-")
    pcTempFilePrefix :: Text,
    -- | Log message prefix (e.g., "Audio")
    pcLogPrefix :: Text
  }

--------------------------------------------------------------------------------
-- Upload Processing Pipeline

-- | Process a staged upload with the given configuration.
--
-- This is the main entry point for processing uploads. It:
-- 1. Generates a secure token
-- 2. Validates and stores the file
-- 3. Creates the staged upload database record
-- 4. Cleans up on failure
processStagedUpload ::
  ProcessConfig ->
  StorageBackend ->
  Maybe AWS.Env ->
  User.Id ->
  Text -> -- Original filename
  Text -> -- Browser MIME type
  BS.ByteString -> -- File content
  AppM (Either Text (StagedUploads.Token, Text, Text, Int64))
-- Returns: (token, originalName, mimeType, fileSize)
processStagedUpload config backend mAwsEnv userId originalName browserMimeType content = do
  let fileSize = fromIntegral $ BS.length content

  -- Check if file is empty
  if BS.null content
    then pure $ Left "No file selected"
    else do
      -- Generate cryptographically secure token
      token <- generateSecureToken

      -- Store the file with validation (pass token for unique filename)
      storeResult <- storeValidatedFile config backend mAwsEnv token browserMimeType content

      case storeResult of
        Left err -> do
          Log.logInfo (pcLogPrefix config <> " upload failed") (Text.pack $ show err)
          pure $ Left $ "Failed to upload " <> Text.toLower (pcLogPrefix config) <> " file"
        Right (storagePath, mimeType) -> do
          -- Create staged upload record
          let insertData =
                StagedUploads.Insert
                  { StagedUploads.siToken = token,
                    StagedUploads.siUserId = userId,
                    StagedUploads.siOriginalName = originalName,
                    StagedUploads.siStoragePath = storagePath,
                    StagedUploads.siMimeType = mimeType,
                    StagedUploads.siFileSize = fileSize,
                    StagedUploads.siUploadType = pcUploadType config
                  }

          insertResult <- execQuerySpan (StagedUploads.insert insertData)

          case insertResult of
            Left err -> do
              Log.logInfo "Failed to create staged upload record" (Text.pack $ show err)
              -- Clean up the stored file
              _ <- deleteFile backend mAwsEnv storagePath
              Log.logInfo "Cleaned up orphaned file after DB failure" storagePath
              pure $ Left "Failed to process upload"
            Right _uploadId -> do
              Log.logInfo (pcLogPrefix config <> " file staged successfully") (StagedUploads.unToken token)
              pure $ Right (token, originalName, mimeType, fileSize)

--------------------------------------------------------------------------------
-- File Storage

-- | Store a file with MIME validation to a flat staging area.
--
-- Staged files are stored without date hierarchy since the final location
-- (with correct air date) is determined when the upload is claimed.
storeValidatedFile ::
  (MonadIO m, MonadMask m, Log.MonadLog m) =>
  ProcessConfig ->
  StorageBackend ->
  Maybe AWS.Env ->
  StagedUploads.Token -> -- Token for unique filename
  Text -> -- Browser MIME type
  BS.ByteString -> -- Content
  m (Either UploadError (Text, Text))
storeValidatedFile config backend mAwsEnv token browserMimeType content = do
  let filename = Text.unpack (pcTempFilePrefix config) <> "upload"

  -- Write to temp file for MIME validation
  Temp.withSystemTempFile filename $ \tempPath handle -> do
    liftIO $ BS.hPut handle content
    liftIO $ hClose handle

    -- Validate the file
    mimeResult <- pcValidator config tempPath browserMimeType

    case mimeResult of
      Left err -> pure $ Left $ UnsupportedFileType err
      Right actualMimeType -> do
        -- Use token UUID for unique filename (cryptographically secure)
        let extension = pcExtensionMapper config actualMimeType
            tokenText = StagedUploads.unToken token
            uniqueFilename = "staged_" <> tokenText <> "." <> extension

        -- Store the file to flat staging area
        storeResult <- storeFileStaging backend mAwsEnv TempBucket "staging" uniqueFilename content actualMimeType

        case storeResult of
          Left err -> pure $ Left $ StorageError err
          Right storagePath -> pure $ Right (storagePath, actualMimeType)

--------------------------------------------------------------------------------
-- Helpers

-- | Store a file to a flat staging area using the configured storage backend.
--
-- Unlike 'storeFile', this does not use date hierarchy - files are stored
-- in a flat structure and will be moved to the correct location when claimed.
storeFileStaging ::
  (MonadIO m) =>
  StorageBackend ->
  Maybe AWS.Env ->
  BucketType ->
  -- | Subdirectory within bucket (e.g., "staging")
  Text ->
  Text ->
  BS.ByteString ->
  Text ->
  m (Either Text Text)
storeFileStaging backend mAwsEnv bucketType subdir filename content mimeType =
  case backend of
    LocalStorage config -> do
      result <- Local.storeFileStagingLocal config bucketType subdir filename content
      pure $ case result of
        Left err -> Left err
        Right objectKey -> Right objectKey
    S3Storage config -> case mAwsEnv of
      Nothing -> pure $ Left "S3 storage requires AWS environment"
      Just awsEnv -> do
        result <- S3.storeFileStagingS3 awsEnv config bucketType subdir filename content mimeType
        pure $ case result of
          Left err -> Left err
          Right objectKey -> Right objectKey
