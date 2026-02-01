{-# LANGUAGE ScopedTypeVariables #-}

module Effects.FileUpload
  ( -- * File upload functions
    uploadEpisodeArtwork,
    uploadShowLogo,
    uploadBlogHeroImage,
    uploadEventPosterImage,
    uploadUserAvatar,

    -- * Helper functions
    isEmptyUpload,
  )
where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime, getCurrentTime)
import Domain.Types.FileStorage
import Domain.Types.FileUpload
import Domain.Types.FileUpload qualified as FileUpload
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend
import Effects.MimeTypeValidation qualified as MimeValidation
import Effects.Storage.Local qualified as Local
import Effects.Storage.S3 qualified as S3
import Log qualified
import Servant.Multipart (FileData, Mem, fdFileCType, fdFileName, fdPayload)
import System.IO (hClose)
import System.IO.Temp qualified as Temp
import System.Random qualified as Random

--------------------------------------------------------------------------------

-- | Upload artwork for an episode.
--
-- Validates the image file using both browser-provided MIME type and magic byte detection,
-- then stores it using the configured storage backend.
--
-- Returns 'Nothing' if no file was provided (empty upload).
-- Artwork files are optional for episodes but when provided must pass validation.
--
-- Filename format matches audio files: {showSlug}_{YYYY-MM-DD}_{hash}.{extension}
uploadEpisodeArtwork ::
  (MonadIO m, MonadMask m, Log.MonadLog m) =>
  -- | Storage backend configuration
  StorageBackend ->
  -- | AWS environment (required for S3, Nothing for local)
  Maybe AWS.Env ->
  -- | Show slug for directory organization
  Slug ->
  -- | Scheduled date (Nothing uses current time)
  Maybe UTCTime ->
  -- | Uploaded artwork file data
  FileData Mem ->
  m (Either UploadError (Maybe UploadResult))
uploadEpisodeArtwork backend mAwsEnv showSlug mScheduledDate fileData
  | isEmptyUpload fileData = pure (Right Nothing)
  | otherwise =
      withTempUpload fileData $ \tempPath content -> runExceptT $ do
        let originalName = fdFileName fileData
            browserMimeType = fdFileCType fileData
            fileSize = fromIntegral $ BS.length content

        -- Get time and random seed
        time <- liftIO $ maybe getCurrentTime pure mScheduledDate
        seed <- liftIO Random.getStdGen

        -- Validate with browser-provided MIME type and file size
        liftEither $ validateUpload ImageBucket originalName browserMimeType fileSize

        -- Validate actual file content against magic bytes
        actualMimeType <- ExceptT $ do
          either (Left . UnsupportedFileType) Right <$> MimeValidation.validateImageFile tempPath browserMimeType

        -- Generate filename and date hierarchy (matching audio file format)
        let dateHier = dateHierarchyFromTime time
            dateStr = dateYear dateHier <> "-" <> dateMonth dateHier <> "-" <> dateDay dateHier
            extension = getExtensionFromMimeType actualMimeType
            filename = generateUniqueFilename (display showSlug <> "_" <> dateStr) extension seed

        -- Store file using appropriate backend
        objectKey <- ExceptT $ storeFile backend mAwsEnv ImageBucket EpisodeArtwork dateHier filename content actualMimeType

        pure $
          Just
            UploadResult
              { uploadResultOriginalName = originalName,
                uploadResultStoragePath = Text.unpack objectKey,
                uploadResultMimeType = actualMimeType,
                uploadResultFileSize = fileSize
              }

-- | Upload a logo image for a show.
--
-- Validates the image file using both browser-provided MIME type and magic byte detection,
-- then stores it using the configured storage backend.
--
-- Returns 'Nothing' if no file was provided (empty upload).
-- Logo files are optional for shows but when provided must pass validation.
uploadShowLogo ::
  (MonadIO m, MonadMask m, Log.MonadLog m) =>
  -- | Storage backend configuration
  StorageBackend ->
  -- | AWS environment (required for S3, Nothing for local)
  Maybe AWS.Env ->
  -- | Show slug for directory organization
  Slug ->
  -- | Uploaded logo file data
  FileData Mem ->
  m (Either UploadError (Maybe UploadResult))
uploadShowLogo backend mAwsEnv showSlug fileData
  | isEmptyUpload fileData = pure (Right Nothing)
  | otherwise =
      withTempUpload fileData $ \tempPath content -> runExceptT $ do
        let originalName = fdFileName fileData
            browserMimeType = fdFileCType fileData
            fileSize = fromIntegral $ BS.length content

        -- Get time and random seed
        time <- liftIO getCurrentTime
        seed <- liftIO Random.getStdGen

        -- Validate with browser-provided MIME type and file size
        liftEither $ validateUpload ImageBucket originalName browserMimeType fileSize

        -- Validate actual file content against magic bytes
        actualMimeType <- ExceptT $ do
          either (Left . UnsupportedFileType) Right <$> MimeValidation.validateImageFile tempPath browserMimeType

        -- Generate filename and date hierarchy
        let dateHier = dateHierarchyFromTime time
            extension = getExtensionFromMimeType actualMimeType
            filename = generateUniqueFilename (display showSlug <> "-logo") extension seed

        -- Store file using appropriate backend
        objectKey <- ExceptT $ storeFile backend mAwsEnv ImageBucket ShowLogo dateHier filename content actualMimeType

        pure $
          Just
            UploadResult
              { uploadResultOriginalName = originalName,
                uploadResultStoragePath = Text.unpack objectKey,
                uploadResultMimeType = actualMimeType,
                uploadResultFileSize = fileSize
              }

-- | Upload a hero image for a blog post.
--
-- Validates the image file using both browser-provided MIME type and magic byte detection,
-- then stores it using the configured storage backend.
--
-- Returns 'Nothing' if no file was provided (empty upload).
-- Hero images are optional for blog posts but when provided must pass validation.
uploadBlogHeroImage ::
  (MonadIO m, MonadMask m, Log.MonadLog m) =>
  -- | Storage backend configuration
  StorageBackend ->
  -- | AWS environment (required for S3, Nothing for local)
  Maybe AWS.Env ->
  -- | Post slug for directory organization
  Slug ->
  -- | Uploaded hero image file data
  FileData Mem ->
  m (Either UploadError (Maybe UploadResult))
uploadBlogHeroImage backend mAwsEnv postSlug fileData
  | isEmptyUpload fileData = pure (Right Nothing)
  | otherwise =
      withTempUpload fileData $ \tempPath content -> runExceptT $ do
        let originalName = fdFileName fileData
            browserMimeType = fdFileCType fileData
            fileSize = fromIntegral $ BS.length content

        -- Get time and random seed
        time <- liftIO getCurrentTime
        seed <- liftIO Random.getStdGen

        -- Validate with browser-provided MIME type and file size
        liftEither $ validateUpload ImageBucket originalName browserMimeType fileSize

        -- Validate actual file content against magic bytes
        actualMimeType <- ExceptT $ do
          either (Left . UnsupportedFileType) Right <$> MimeValidation.validateImageFile tempPath browserMimeType

        -- Generate filename and date hierarchy
        let dateHier = dateHierarchyFromTime time
            extension = getExtensionFromMimeType actualMimeType
            filename = generateUniqueFilename (display postSlug <> "-hero") extension seed

        -- Store file using appropriate backend
        objectKey <- ExceptT $ storeFile backend mAwsEnv ImageBucket BlogHeroImage dateHier filename content actualMimeType

        pure $
          Just
            UploadResult
              { uploadResultOriginalName = originalName,
                uploadResultStoragePath = Text.unpack objectKey,
                uploadResultMimeType = actualMimeType,
                uploadResultFileSize = fileSize
              }

-- | Upload a poster image for an event.
--
-- Validates the image file using both browser-provided MIME type and magic byte detection,
-- then stores it using the configured storage backend.
--
-- Returns 'Nothing' if no file was provided (empty upload).
-- Poster images are optional for events but when provided must pass validation.
uploadEventPosterImage ::
  (MonadIO m, MonadMask m, Log.MonadLog m) =>
  -- | Storage backend configuration
  StorageBackend ->
  -- | AWS environment (required for S3, Nothing for local)
  Maybe AWS.Env ->
  -- | Event slug for directory organization
  Slug ->
  -- | Uploaded poster image file data
  FileData Mem ->
  m (Either UploadError (Maybe UploadResult))
uploadEventPosterImage backend mAwsEnv eventSlug fileData
  | isEmptyUpload fileData = pure (Right Nothing)
  | otherwise =
      withTempUpload fileData $ \tempPath content -> runExceptT $ do
        let originalName = fdFileName fileData
            browserMimeType = fdFileCType fileData
            fileSize = fromIntegral $ BS.length content

        -- Get time and random seed
        time <- liftIO getCurrentTime
        seed <- liftIO Random.getStdGen

        -- Validate with browser-provided MIME type and file size
        liftEither $ validateUpload ImageBucket originalName browserMimeType fileSize

        -- Validate actual file content against magic bytes
        actualMimeType <- ExceptT $ do
          either (Left . UnsupportedFileType) Right <$> MimeValidation.validateImageFile tempPath browserMimeType

        -- Generate filename and date hierarchy
        let dateHier = dateHierarchyFromTime time
            extension = getExtensionFromMimeType actualMimeType
            filename = generateUniqueFilename (display eventSlug <> "-poster") extension seed

        -- Store file using appropriate backend
        objectKey <- ExceptT $ storeFile backend mAwsEnv ImageBucket EventPosterImage dateHier filename content actualMimeType

        pure $
          Just
            UploadResult
              { uploadResultOriginalName = originalName,
                uploadResultStoragePath = Text.unpack objectKey,
                uploadResultMimeType = actualMimeType,
                uploadResultFileSize = fileSize
              }

-- | Upload an avatar image for a user.
--
-- Validates the image file using both browser-provided MIME type and magic byte detection,
-- then stores it using the configured storage backend.
--
-- Returns 'Nothing' if no file was provided (empty upload).
-- Avatar images are optional for users but when provided must pass validation.
uploadUserAvatar ::
  (MonadIO m, MonadMask m, Log.MonadLog m) =>
  -- | Storage backend configuration
  StorageBackend ->
  -- | AWS environment (required for S3, Nothing for local)
  Maybe AWS.Env ->
  -- | User ID for directory organization
  Text ->
  -- | Uploaded avatar file data
  FileData Mem ->
  m (Either UploadError (Maybe UploadResult))
uploadUserAvatar backend mAwsEnv userId fileData
  | isEmptyUpload fileData = pure (Right Nothing)
  | otherwise =
      withTempUpload fileData $ \tempPath content -> runExceptT $ do
        let originalName = fdFileName fileData
            browserMimeType = fdFileCType fileData
            fileSize = fromIntegral $ BS.length content

        -- Get time and random seed
        time <- liftIO getCurrentTime
        seed <- liftIO Random.getStdGen

        -- Validate with browser-provided MIME type and file size
        liftEither $ validateUpload ImageBucket originalName browserMimeType fileSize

        -- Validate actual file content against magic bytes
        actualMimeType <- ExceptT $ do
          either (Left . UnsupportedFileType) Right <$> MimeValidation.validateImageFile tempPath browserMimeType

        -- Generate filename and date hierarchy
        let dateHier = dateHierarchyFromTime time
            extension = getExtensionFromMimeType actualMimeType
            filename = generateUniqueFilename ("user-" <> userId <> "-avatar") extension seed

        -- Store file using appropriate backend
        objectKey <- ExceptT $ storeFile backend mAwsEnv ImageBucket UserAvatar dateHier filename content actualMimeType

        pure $
          Just
            UploadResult
              { uploadResultOriginalName = originalName,
                uploadResultStoragePath = Text.unpack objectKey,
                uploadResultMimeType = actualMimeType,
                uploadResultFileSize = fileSize
              }

--------------------------------------------------------------------------------
-- Internal helper functions

-- | Store a file using the configured storage backend.
--
-- Dispatches to either local filesystem storage or S3 based on the backend configuration.
storeFile ::
  (MonadIO m) =>
  StorageBackend ->
  Maybe AWS.Env ->
  BucketType ->
  ResourceType ->
  DateHierarchy ->
  Text ->
  BS.ByteString ->
  Text ->
  m (Either UploadError Text)
storeFile backend mAwsEnv bucketType resourceType dateHier filename content mimeType =
  case backend of
    LocalStorage config -> do
      result <- Local.storeFileLocal config bucketType resourceType dateHier filename content
      pure $ case result of
        Left err -> Left $ StorageError err
        Right objectKey -> Right objectKey
    S3Storage config -> case mAwsEnv of
      Nothing -> pure $ Left $ StorageError "S3 storage requires AWS environment"
      Just awsEnv -> do
        result <- S3.storeFileS3 awsEnv config bucketType resourceType dateHier filename content mimeType
        pure $ case result of
          Left err -> Left $ StorageError err
          Right objectKey -> Right objectKey

-- | Execute an action with a temporary file, ensuring cleanup.
--
-- Takes the in-memory file data from a multipart form upload, writes it
-- to a temporary file, runs the provided action, and ensures the temp file
-- is deleted regardless of success or failure.
--
-- The temporary file is created in @\/tmp\/@ with a prefix to avoid conflicts.
-- The filename is sanitized to remove any invalid characters (colons, slashes, etc.)
-- that might cause issues with the filesystem.
--
-- The action receives both the temp file path (for validation) and the file
-- content (to avoid reading the file twice).
withTempUpload :: (MonadIO m, MonadMask m) => FileData Mem -> (FilePath -> BS.ByteString -> m a) -> m a
withTempUpload fileData action = do
  let content = BSL.toStrict $ fdPayload fileData
      -- Sanitize the filename to avoid issues with special characters (e.g., colons in timestamps)
      sanitizedName = FileUpload.sanitizeFileName (fdFileName fileData)
      filename = "kpbj-upload-" <> Text.unpack sanitizedName
  Temp.withSystemTempFile
    filename
    ( \path handle -> do
        -- Write content to the temp file for MIME validation
        liftIO $ BS.hPut handle content
        liftIO $ hClose handle
        action path content
    )

-- | Get file extension from MIME type.
getExtensionFromMimeType :: Text -> Text
getExtensionFromMimeType mimeType = case mimeType of
  "audio/mpeg" -> "mp3"
  "audio/wav" -> "wav"
  "audio/flac" -> "flac"
  "audio/aac" -> "aac"
  "audio/ogg" -> "ogg"
  "audio/x-m4a" -> "m4a"
  "image/jpeg" -> "jpg"
  "image/jpg" -> "jpg"
  "image/png" -> "png"
  "image/webp" -> "webp"
  "image/gif" -> "gif"
  _ -> "bin"

-- | Check if a file upload is empty (no file selected).
--
-- When a form has a file input but no file is selected, the browser sends
-- an empty payload with content-type "application/octet-stream". This helper
-- detects that case so handlers can skip processing.
isEmptyUpload :: FileData Mem -> Bool
isEmptyUpload fileData = BSL.null (fdPayload fileData)
