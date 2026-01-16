{-# LANGUAGE ScopedTypeVariables #-}

-- | Cleanup functionality for expired staged uploads.
--
-- Staged uploads that are not claimed within 24 hours are considered orphaned
-- and should be cleaned up to prevent storage leaks.
module Effects.StagedUploadCleanup
  ( -- * Cleanup Functions
    cleanupExpiredUploads,

    -- * File Deletion
    deleteFile,
  )
where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import Control.Exception qualified
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Has (Has)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.StorageBackend (LocalStorageConfig (..), StorageBackend (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Storage.S3 qualified as S3
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import OpenTelemetry.Trace (Tracer)
import System.Directory (removeFile)
import System.FilePath ((</>))

--------------------------------------------------------------------------------

-- | Clean up expired staged uploads.
--
-- This function:
-- 1. Queries for all uploads with status 'pending' and expires_at < NOW()
-- 2. Deletes the file from storage for each expired upload
-- 3. Deletes the database record
--
-- Returns the count of cleaned up uploads.
cleanupExpiredUploads ::
  ( MonadIO m,
    MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    Has StorageBackend env,
    Has (Maybe AWS.Env) env
  ) =>
  m Int
cleanupExpiredUploads = do
  backend <- asks (Has.getter @StorageBackend)
  mAwsEnv <- asks (Has.getter @(Maybe AWS.Env))

  -- Get expired uploads
  result <- execQuerySpan StagedUploads.getExpiredUploads
  case result of
    Left err -> do
      Log.logInfo "Failed to query expired uploads" (Text.pack $ show err)
      pure 0
    Right [] -> do
      Log.logInfo "No expired uploads to clean up" ("" :: Text)
      pure 0
    Right uploads -> do
      Log.logInfo "Found expired uploads to clean up" (Text.pack $ show (length uploads))
      cleanupCount <- cleanupUploads backend mAwsEnv uploads
      Log.logInfo "Cleaned up expired uploads" (Text.pack $ show cleanupCount)
      pure cleanupCount

-- | Clean up a list of expired uploads.
cleanupUploads ::
  ( MonadIO m,
    MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env
  ) =>
  StorageBackend ->
  Maybe AWS.Env ->
  [StagedUploads.Model] ->
  m Int
cleanupUploads backend mAwsEnv uploads = do
  cleanedCount <- go 0 uploads
  pure cleanedCount
  where
    go count [] = pure count
    go count (upload : rest) = do
      success <- cleanupSingleUpload backend mAwsEnv upload
      go (if success then count + 1 else count) rest

-- | Clean up a single expired upload.
cleanupSingleUpload ::
  ( MonadIO m,
    MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env
  ) =>
  StorageBackend ->
  Maybe AWS.Env ->
  StagedUploads.Model ->
  m Bool
cleanupSingleUpload backend mAwsEnv upload = do
  let uploadId = StagedUploads.id upload
      storagePath = StagedUploads.storagePath upload

  -- Delete file from storage
  fileDeleted <- deleteFile backend mAwsEnv storagePath

  if fileDeleted
    then do
      -- Delete database record
      deleteResult <- execQuerySpan (StagedUploads.deleteById uploadId)
      case deleteResult of
        Left err -> do
          Log.logInfo "Failed to delete staged upload record" (Text.pack $ show err)
          pure False
        Right Nothing -> do
          Log.logInfo "Staged upload record not found" (Text.pack $ show uploadId)
          pure False
        Right (Just _) -> do
          Log.logInfo "Cleaned up expired upload" (StagedUploads.unToken (StagedUploads.token upload))
          pure True
    else do
      Log.logInfo "Failed to delete staged upload file" storagePath
      -- Still delete the database record even if file deletion failed
      _ <- execQuerySpan (StagedUploads.deleteById uploadId)
      pure False

-- | Delete a file from storage.
deleteFile ::
  (MonadIO m) =>
  StorageBackend ->
  Maybe AWS.Env ->
  Text ->
  m Bool
deleteFile backend mAwsEnv storagePath =
  case backend of
    LocalStorage config -> do
      let fullPath = localStorageRoot config </> Text.unpack storagePath
      liftIO $ deleteFileLocal fullPath
    S3Storage config -> case mAwsEnv of
      Nothing -> pure False
      Just awsEnv -> do
        result <- S3.deleteFileS3 awsEnv config storagePath
        case result of
          Left _ -> pure False
          Right _ -> pure True

-- | Delete a local file, returning True on success.
deleteFileLocal :: FilePath -> IO Bool
deleteFileLocal path = do
  result <- Control.Exception.try (removeFile path)
  case result of
    Left (_ :: Control.Exception.IOException) -> pure False
    Right () -> pure True
