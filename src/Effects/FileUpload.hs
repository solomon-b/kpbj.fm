module Effects.FileUpload
  ( -- * File upload functions
    uploadEpisodeAudio,
    uploadEpisodeArtwork,
    uploadShowLogo,
    uploadShowBanner,
    uploadBlogHeroImage,

    -- * Helper functions
    stripStorageRoot,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Domain.Types.FileStorage
import Domain.Types.FileUpload
import Domain.Types.Slug (Slug)
import Effects.MimeTypeValidation qualified as MimeValidation
import Log qualified
import Servant.Multipart (FileData, Mem, fdFileCType, fdFileName, fdPayload)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Random qualified as Random

--------------------------------------------------------------------------------

-- | Upload an audio file for an episode.
--
-- Validates the file using both browser-provided MIME type and magic byte detection,
-- then stores it in the episode audio directory organized by show and date.
--
-- Audio files are required for episodes and validation failures will cause the upload to fail.
uploadEpisodeAudio ::
  (MonadIO m, Log.MonadLog m) =>
  -- | Show slug for directory organization
  Slug ->
  -- | Episode slug for filename generation
  Slug ->
  -- | Scheduled date (Nothing uses current time)
  Maybe UTCTime ->
  -- | Uploaded audio file data
  FileData Mem ->
  m (Either UploadError UploadResult)
uploadEpisodeAudio showSlug episodeSlug mScheduledDate fileData = runExceptT $ do
  -- Convert to temp file and process
  tempPath <- ExceptT $ convertFileDataToTempFile fileData

  let originalName = fdFileName fileData
      browserMimeType = fdFileCType fileData

  -- Get file info and setup
  fileSize <- liftIO $ getFileSize tempPath
  let config = defaultStorageConfig
  time <- liftIO $ maybe getCurrentTime pure mScheduledDate
  seed <- liftIO Random.getStdGen

  -- Validate with browser-provided MIME type and file size
  liftEither $ validateUpload AudioBucket originalName browserMimeType fileSize

  -- Validate actual file content against magic bytes
  actualMimeType <- ExceptT $ do
    either (Left . UnsupportedFileType) Right <$> MimeValidation.validateAudioFile tempPath browserMimeType

  -- Build upload result and move file
  let uploadResult = buildEpisodeAudioUpload config showSlug episodeSlug originalName actualMimeType fileSize time seed
      storagePath = uploadResultStoragePath uploadResult

  liftIO $ do
    createDirectoryIfMissing True (takeDirectory storagePath)
    copyFile tempPath storagePath

  pure uploadResult

-- | Upload artwork for an episode.
--
-- Validates the image file using both browser-provided MIME type and magic byte detection,
-- then stores it in the episode artwork directory organized by show and date.
--
-- Artwork files are optional for episodes but when provided must pass validation.
uploadEpisodeArtwork ::
  (MonadIO m, Log.MonadLog m) =>
  -- | Show slug for directory organization
  Slug ->
  -- | Episode slug for filename generation
  Slug ->
  -- | Scheduled date (Nothing uses current time)
  Maybe UTCTime ->
  -- | Uploaded artwork file data
  FileData Mem ->
  m (Either UploadError UploadResult)
uploadEpisodeArtwork showSlug episodeSlug mScheduledDate fileData = runExceptT $ do
  -- Convert to temp file and process
  tempPath <- ExceptT $ convertFileDataToTempFile fileData

  let originalName = fdFileName fileData
      browserMimeType = fdFileCType fileData

  -- Get file info and setup
  fileSize <- liftIO $ getFileSize tempPath
  let config = defaultStorageConfig
  time <- liftIO $ maybe getCurrentTime pure mScheduledDate
  seed <- liftIO Random.getStdGen

  -- Validate with browser-provided MIME type and file size
  liftEither $ validateUpload ImageBucket originalName browserMimeType fileSize

  -- Validate actual file content against magic bytes
  actualMimeType <- ExceptT $ do
    either (Left . UnsupportedFileType) Right <$> MimeValidation.validateImageFile tempPath browserMimeType

  -- Build upload result and move file
  let uploadResult = buildEpisodeArtworkUpload config showSlug episodeSlug originalName actualMimeType fileSize time seed
      storagePath = uploadResultStoragePath uploadResult

  liftIO $ do
    createDirectoryIfMissing True (takeDirectory storagePath)
    copyFile tempPath storagePath

  pure uploadResult

-- | Upload a logo image for a show.
--
-- Validates the image file using both browser-provided MIME type and magic byte detection,
-- then stores it in the show assets directory.
--
-- Logo files are optional for shows but when provided must pass validation.
uploadShowLogo ::
  (MonadIO m, Log.MonadLog m) =>
  -- | Show slug for directory organization
  Slug ->
  -- | Uploaded logo file data
  FileData Mem ->
  m (Either UploadError UploadResult)
uploadShowLogo showSlug fileData = runExceptT $ do
  -- Convert to temp file and process
  tempPath <- ExceptT $ convertFileDataToTempFile fileData

  let originalName = fdFileName fileData
      browserMimeType = fdFileCType fileData

  -- Get file info and setup
  fileSize <- liftIO $ getFileSize tempPath
  let config = defaultStorageConfig
  time <- liftIO getCurrentTime
  seed <- liftIO Random.getStdGen

  -- Validate with browser-provided MIME type and file size
  liftEither $ validateUpload ImageBucket originalName browserMimeType fileSize

  -- Validate actual file content against magic bytes
  actualMimeType <- ExceptT $ do
    either (Left . UnsupportedFileType) Right <$> MimeValidation.validateImageFile tempPath browserMimeType

  -- Build upload result and move file
  let uploadResult = buildShowLogoUpload config showSlug originalName actualMimeType fileSize time seed
      storagePath = uploadResultStoragePath uploadResult

  liftIO $ do
    createDirectoryIfMissing True (takeDirectory storagePath)
    copyFile tempPath storagePath

  pure uploadResult

-- | Upload a banner image for a show.
--
-- Validates the image file using both browser-provided MIME type and magic byte detection,
-- then stores it in the show assets directory.
--
-- Banner files are optional for shows but when provided must pass validation.
uploadShowBanner ::
  (MonadIO m, Log.MonadLog m) =>
  -- | Show slug for directory organization
  Slug ->
  -- | Uploaded banner file data
  FileData Mem ->
  m (Either UploadError UploadResult)
uploadShowBanner showSlug fileData = runExceptT $ do
  -- Convert to temp file and process
  tempPath <- ExceptT $ convertFileDataToTempFile fileData

  let originalName = fdFileName fileData
      browserMimeType = fdFileCType fileData

  -- Get file info and setup
  fileSize <- liftIO $ getFileSize tempPath
  let config = defaultStorageConfig
  time <- liftIO getCurrentTime
  seed <- liftIO Random.getStdGen

  -- Validate with browser-provided MIME type and file size
  liftEither $ validateUpload ImageBucket originalName browserMimeType fileSize

  -- Validate actual file content against magic bytes
  actualMimeType <- ExceptT $ do
    either (Left . UnsupportedFileType) Right <$> MimeValidation.validateImageFile tempPath browserMimeType

  -- Build upload result and move file
  let uploadResult = buildShowBannerUpload config showSlug originalName actualMimeType fileSize time seed
      storagePath = uploadResultStoragePath uploadResult

  liftIO $ do
    createDirectoryIfMissing True (takeDirectory storagePath)
    copyFile tempPath storagePath

  pure uploadResult

-- | Upload a hero image for a blog post.
--
-- Validates the image file using both browser-provided MIME type and magic byte detection,
-- then stores it in the blog hero images directory.
--
-- Hero images are optional for blog posts but when provided must pass validation.
uploadBlogHeroImage ::
  (MonadIO m, Log.MonadLog m) =>
  -- | Post slug for directory organization
  Slug ->
  -- | Uploaded hero image file data
  FileData Mem ->
  m (Either UploadError UploadResult)
uploadBlogHeroImage postSlug fileData = runExceptT $ do
  -- Convert to temp file and process
  tempPath <- ExceptT $ convertFileDataToTempFile fileData

  let originalName = fdFileName fileData
      browserMimeType = fdFileCType fileData

  -- Get file info and setup
  fileSize <- liftIO $ getFileSize tempPath
  let config = defaultStorageConfig
  time <- liftIO getCurrentTime
  seed <- liftIO Random.getStdGen

  -- Validate with browser-provided MIME type and file size
  liftEither $ validateUpload ImageBucket originalName browserMimeType fileSize

  -- Validate actual file content against magic bytes
  actualMimeType <- ExceptT $ do
    either (Left . UnsupportedFileType) Right <$> MimeValidation.validateImageFile tempPath browserMimeType

  -- Build upload result and move file
  let uploadResult = buildBlogHeroImageUpload config postSlug originalName actualMimeType fileSize time seed
      storagePath = uploadResultStoragePath uploadResult

  liftIO $ do
    createDirectoryIfMissing True (takeDirectory storagePath)
    copyFile tempPath storagePath

  pure uploadResult

--------------------------------------------------------------------------------
-- Helper functions

-- | Convert uploaded file data to a temporary file on disk.
--
-- Takes the in-memory file data from a multipart form upload and writes it
-- to a temporary file that can be processed by validation and storage functions.
--
-- The temporary file is created in @\/tmp\/@ with a prefix to avoid conflicts.
convertFileDataToTempFile :: (MonadIO m) => FileData Mem -> m (Either UploadError FilePath)
convertFileDataToTempFile fileData = liftIO $ do
  let content = BSL.toStrict $ fdPayload fileData
      tempName = "/tmp/kpbj-upload-" <> Text.unpack (fdFileName fileData)
  -- Write directly to temp file
  BS.writeFile tempName content
  pure $ Right tempName

-- | Get the size of a file in bytes.
--
-- Reads the entire file into memory to determine its size.
-- For large files, this could be memory-intensive.
getFileSize :: FilePath -> IO Int64
getFileSize path = do
  content <- BS.readFile path
  pure $ fromIntegral $ BS.length content

-- | Strip storage root from file path to get relative path.
--
-- Converts absolute paths like @\/tmp\/kpbj\/images\/...@ to relative paths like @images\/...@
-- This is needed because the static file server already serves from @\/tmp\/kpbj\/@
--
-- If the path doesn't start with the prefix, returns the original path unchanged.
stripStorageRoot :: FilePath -> Text
stripStorageRoot path =
  let prefix = "/tmp/kpbj/"
   in case List.stripPrefix prefix path of
        Just relativePath -> Text.pack relativePath
        Nothing -> Text.pack path
