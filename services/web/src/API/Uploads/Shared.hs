{-# LANGUAGE ScopedTypeVariables #-}

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

    -- * Error Handling
    StagedUploadError (..),
    displayStagedUploadError,

    -- * File Validation
    FileValidator,

    -- * Extension Mapping
    ExtensionMapper,
  )
where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import App.Monad (AppM)
import Control.Exception qualified as Exception
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.FileStorage (BucketType (..))
import Domain.Types.FileUpload (UploadError (..))
import Domain.Types.StorageBackend (StorageBackend (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.StagedUploadCleanup (deleteFile)
import Effects.StagedUploads (generateSecureToken)
import Effects.Storage.Local qualified as Local (storeFileStagingLocalFromFile)
import Effects.Storage.S3 qualified as S3 (storeFileStagingS3)
import Log qualified
import System.Directory (getFileSize)

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
    -- | Log message prefix (e.g., "Audio")
    pcLogPrefix :: Text
  }

-- | Structured errors for the staged upload pipeline.
data StagedUploadError
  = -- | The uploaded file was empty (0 bytes).
    EmptyFile
  | -- | An IO error occurred reading the temp file (e.g., getFileSize failed).
    FileSystemError Text
  | -- | MIME type validation rejected the file.
    MimeValidationFailed Text
  | -- | Storage backend failed to store the file.
    StorageFailed Text
  | -- | Database insert failed after file was stored (orphaned file cleaned up).
    DatabaseInsertFailed Text
  deriving (Show)

-- | User-facing error message for a staged upload error.
displayStagedUploadError ::
  -- | Upload type prefix (e.g., "Audio")
  Text ->
  StagedUploadError ->
  Text
displayStagedUploadError prefix = \case
  EmptyFile -> "No file selected"
  FileSystemError _ -> "Failed to process upload"
  MimeValidationFailed _ -> "Failed to upload " <> Text.toLower prefix <> " file"
  StorageFailed _ -> "Failed to upload " <> Text.toLower prefix <> " file"
  DatabaseInsertFailed _ -> "Failed to process upload"

-- | Log a staged upload error with appropriate severity.
--
-- Uses 'logAttention' for unexpected failures (filesystem, storage, DB)
-- and 'logInfo' for expected rejections (empty file, MIME validation).
logStagedUploadError :: Text -> StagedUploadError -> AppM ()
logStagedUploadError prefix = \case
  EmptyFile -> Log.logInfo (prefix <> " upload rejected") ("empty file" :: Text)
  FileSystemError err -> Log.logAttention (prefix <> " filesystem error") err
  MimeValidationFailed err -> Log.logInfo (prefix <> " upload failed") err
  StorageFailed err -> Log.logAttention (prefix <> " storage failed") err
  DatabaseInsertFailed err -> Log.logAttention (prefix <> " database insert failed") err

--------------------------------------------------------------------------------
-- Upload Processing Pipeline

-- | Process a staged upload with the given configuration.
--
-- This is the main entry point for processing uploads. It:
-- 1. Generates a secure token
-- 2. Validates and stores the file
-- 3. Creates the staged upload database record
-- 4. Cleans up on failure
--
-- The file path should point to a temporary file written by the Servant
-- multipart 'Tmp' backend. The file is read from disk for storage,
-- avoiding buffering the entire upload in memory during request parsing.
processStagedUpload ::
  ProcessConfig ->
  StorageBackend ->
  Maybe AWS.Env ->
  User.Id ->
  -- | Original filename
  Text ->
  -- | Browser MIME type
  Text ->
  -- | Path to temp file (from Servant Tmp backend)
  FilePath ->
  AppM (Either StagedUploadError (StagedUploads.Token, Text, Text, Int64))
-- Returns: (token, originalName, mimeType, fileSize)
processStagedUpload config backend mAwsEnv userId originalName browserMimeType tempFilePath = do
  result <- runExceptT $ do
    fileSize <- getFileSizeE tempFilePath
    when (fileSize == 0) $ throwE EmptyFile

    token <- lift generateSecureToken

    (storagePath, mimeType) <-
      storeValidatedFileE config backend mAwsEnv token browserMimeType tempFilePath

    insertStagedUploadE backend mAwsEnv storagePath $
      StagedUploads.Insert
        { StagedUploads.siToken = token,
          StagedUploads.siUserId = userId,
          StagedUploads.siOriginalName = originalName,
          StagedUploads.siStoragePath = storagePath,
          StagedUploads.siMimeType = mimeType,
          StagedUploads.siFileSize = fileSize,
          StagedUploads.siUploadType = pcUploadType config
        }

    pure (token, originalName, mimeType, fileSize)

  -- Centralized logging
  case result of
    Left err -> logStagedUploadError (pcLogPrefix config) err
    Right (token, _, _, _) ->
      Log.logInfo (pcLogPrefix config <> " file staged successfully") (StagedUploads.unToken token)

  pure result

--------------------------------------------------------------------------------
-- ExceptT Helpers (private)

-- | Get file size, catching IOExceptions.
getFileSizeE :: FilePath -> ExceptT StagedUploadError AppM Int64
getFileSizeE path = do
  result <- liftIO $ Exception.try $ getFileSize path
  case result of
    Left (err :: Exception.IOException) ->
      throwE $ FileSystemError (Text.pack $ show err)
    Right size ->
      pure (fromIntegral size)

-- | Validate and store a file, mapping errors to 'StagedUploadError'.
storeValidatedFileE ::
  ProcessConfig ->
  StorageBackend ->
  Maybe AWS.Env ->
  StagedUploads.Token ->
  Text ->
  FilePath ->
  ExceptT StagedUploadError AppM (Text, Text)
storeValidatedFileE config backend mAwsEnv token browserMimeType tempFilePath = do
  result <- lift $ storeValidatedFile config backend mAwsEnv token browserMimeType tempFilePath
  case result of
    Left (UnsupportedFileType err) -> throwE $ MimeValidationFailed err
    Left (StorageError err) -> throwE $ StorageFailed err
    Left (FileTooLarge actual maxSize) -> throwE $ StorageFailed ("File too large: " <> Text.pack (show actual) <> " > " <> Text.pack (show maxSize))
    Left (InvalidFileName err) -> throwE $ StorageFailed err
    Right ok -> pure ok

-- | Insert a staged upload record, cleaning up the stored file on failure.
insertStagedUploadE ::
  StorageBackend ->
  Maybe AWS.Env ->
  -- | Storage path (for cleanup on failure)
  Text ->
  StagedUploads.Insert ->
  ExceptT StagedUploadError AppM ()
insertStagedUploadE backend mAwsEnv storagePath insertData = do
  insertResult <- lift $ execQuery (StagedUploads.insert insertData)
  case insertResult of
    Left err -> do
      _ <- lift $ deleteFile backend mAwsEnv storagePath
      throwE $ DatabaseInsertFailed (Text.pack $ show err)
    Right _id -> pure ()

--------------------------------------------------------------------------------
-- File Storage

-- | Validate and store a file to a flat staging area.
--
-- Validates the file's MIME type directly from the temp file on disk,
-- then stores it. For local storage, the file is copied without loading
-- into memory. For S3, the file is read into memory for upload.
--
-- Staged files are stored without date hierarchy since the final location
-- (with correct air date) is determined when the upload is claimed.
storeValidatedFile ::
  (MonadIO m, Log.MonadLog m) =>
  ProcessConfig ->
  StorageBackend ->
  Maybe AWS.Env ->
  StagedUploads.Token -> -- Token for unique filename
  Text -> -- Browser MIME type
  FilePath -> -- Path to temp file
  m (Either UploadError (Text, Text))
storeValidatedFile config backend mAwsEnv token browserMimeType tempFilePath = do
  -- Validate the file directly from the temp file (no extra copy needed)
  mimeResult <- pcValidator config tempFilePath browserMimeType

  case mimeResult of
    Left err -> pure $ Left $ UnsupportedFileType err
    Right actualMimeType -> do
      -- Use token UUID for unique filename (cryptographically secure)
      let extension = pcExtensionMapper config actualMimeType
          tokenText = StagedUploads.unToken token
          uniqueFilename = "staged_" <> tokenText <> "." <> extension

      -- Store the file to flat staging area
      storeResult <- storeFileStaging backend mAwsEnv TempBucket "staging" uniqueFilename tempFilePath actualMimeType

      case storeResult of
        Left err -> pure $ Left $ StorageError err
        Right storagePath -> pure $ Right (storagePath, actualMimeType)

--------------------------------------------------------------------------------
-- Helpers

-- | Store a file to a flat staging area using the configured storage backend.
--
-- For local storage, copies the file directly (no memory buffering).
-- For S3, reads the file into memory for upload.
storeFileStaging ::
  (MonadIO m) =>
  StorageBackend ->
  Maybe AWS.Env ->
  BucketType ->
  -- | Subdirectory within bucket (e.g., "staging")
  Text ->
  Text ->
  FilePath ->
  Text ->
  m (Either Text Text)
storeFileStaging backend mAwsEnv bucketType subdir filename sourceFilePath mimeType =
  case backend of
    LocalStorage config -> do
      result <- Local.storeFileStagingLocalFromFile config bucketType subdir filename sourceFilePath
      pure $ case result of
        Left err -> Left err
        Right objectKey -> Right objectKey
    S3Storage config -> case mAwsEnv of
      Nothing -> pure $ Left "S3 storage requires AWS environment"
      Just awsEnv -> do
        -- Read file into memory for S3 upload.
        -- TODO: Switch to streaming/chunked upload for large files.
        content <- liftIO $ BS.readFile sourceFilePath
        result <- S3.storeFileStagingS3 awsEnv config bucketType subdir filename content mimeType
        pure $ case result of
          Left err -> Left err
          Right objectKey -> Right objectKey
