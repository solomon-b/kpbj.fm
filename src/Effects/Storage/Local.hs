{-# LANGUAGE ScopedTypeVariables #-}

module Effects.Storage.Local
  ( -- * Local Storage Operations
    storeFileLocal,
    storeFileStagingLocal,
    moveFileLocal,
    buildLocalPath,
    buildLocalStagingPath,
  )
where

--------------------------------------------------------------------------------

import Control.Exception qualified as Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.FileStorage (BucketType, DateHierarchy (..), ResourceType, bucketTypePath, buildStagingKey, buildStorageKey, resourceTypePath)
import Domain.Types.StorageBackend (LocalStorageConfig (..))
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeDirectory, (</>))

--------------------------------------------------------------------------------

-- | Store a file to the local filesystem.
--
-- Creates the necessary directory structure and writes the file content.
-- Returns the object key (relative path) that can be used with 'buildMediaUrl'.
storeFileLocal ::
  (MonadIO m) =>
  LocalStorageConfig ->
  BucketType ->
  ResourceType ->
  DateHierarchy ->
  -- | Filename
  Text ->
  -- | File content
  BS.ByteString ->
  m (Either Text Text)
storeFileLocal config bucketType resourceType dateHier filename content = liftIO $ do
  let fullPath = buildLocalPath config bucketType resourceType dateHier filename
      objectKey = buildStorageKey bucketType resourceType dateHier filename

  -- Create directory structure
  createDirectoryIfMissing True (takeDirectory fullPath)

  -- Write file
  BS.writeFile fullPath content

  pure $ Right objectKey

-- | Build the full local filesystem path for a file.
--
-- Example: /tmp/kpbj/images/2024/01/15/logos/show-slug_abc123.png
buildLocalPath ::
  LocalStorageConfig ->
  BucketType ->
  ResourceType ->
  DateHierarchy ->
  -- | Filename
  Text ->
  FilePath
buildLocalPath config bucketType resourceType dateHier filename =
  localStorageRoot config
    </> Text.unpack (bucketTypePath bucketType)
    </> Text.unpack (dateYear dateHier)
    </> Text.unpack (dateMonth dateHier)
    </> Text.unpack (dateDay dateHier)
    </> Text.unpack (resourceTypePath resourceType)
    </> Text.unpack filename

-- | Store a file to a flat staging area (no date hierarchy).
--
-- Used for staged uploads that will be moved to their final location
-- when claimed.
storeFileStagingLocal ::
  (MonadIO m) =>
  LocalStorageConfig ->
  BucketType ->
  -- | Subdirectory within bucket (e.g., "staging")
  Text ->
  -- | Filename
  Text ->
  -- | File content
  BS.ByteString ->
  m (Either Text Text)
storeFileStagingLocal config bucketType subdir filename content = liftIO $ do
  let fullPath = buildLocalStagingPath config bucketType subdir filename
      objectKey = buildStagingKey bucketType subdir filename

  -- Create directory structure
  createDirectoryIfMissing True (takeDirectory fullPath)

  -- Write file
  BS.writeFile fullPath content

  pure $ Right objectKey

-- | Build the full local filesystem path for a staging file (no date hierarchy).
--
-- Example: /tmp/kpbj/temp/staging/abc123def456.mp3
buildLocalStagingPath ::
  LocalStorageConfig ->
  BucketType ->
  -- | Subdirectory within bucket
  Text ->
  -- | Filename
  Text ->
  FilePath
buildLocalStagingPath config bucketType subdir filename =
  localStorageRoot config
    </> Text.unpack (bucketTypePath bucketType)
    </> Text.unpack subdir
    </> Text.unpack filename

-- | Move a file from one storage location to another.
--
-- Used when claiming staged uploads to move them to their final location
-- with the correct date hierarchy.
moveFileLocal ::
  (MonadIO m) =>
  LocalStorageConfig ->
  -- | Source object key (relative path)
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
moveFileLocal config sourceKey destBucketType destResourceType destDateHier destFilename = liftIO $ do
  let sourcePath = localStorageRoot config </> Text.unpack sourceKey
      destPath = buildLocalPath config destBucketType destResourceType destDateHier destFilename
      destKey = buildStorageKey destBucketType destResourceType destDateHier destFilename

  -- Create destination directory
  createDirectoryIfMissing True (takeDirectory destPath)

  -- Move the file
  result <- Exception.try $ renameFile sourcePath destPath
  case result of
    Left (err :: Exception.IOException) ->
      pure $ Left $ "Failed to move file: " <> Text.pack (show err)
    Right () ->
      pure $ Right destKey
