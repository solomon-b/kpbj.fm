module Effects.FileUpload where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Domain.Types.FileStorage
import Domain.Types.FileUpload
import Domain.Types.Slug (Slug)
import Network.Wai.Parse (FileInfo (..))
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Random qualified as Random

--------------------------------------------------------------------------------

-- | Handle file upload with proper validation and storage
uploadEpisodeAudio ::
  (MonadIO m) =>
  Slug -> -- Show slug
  Slug -> -- Episode slug
  Maybe UTCTime -> -- Scheduled date (or Nothing to use current time)
  FileInfo FilePath -> -- Uploaded file info
  m (Either UploadError UploadResult)
uploadEpisodeAudio showSlug episodeSlug mScheduledDate fileInfo = liftIO $ do
  let originalName = Text.decodeUtf8 $ fileName fileInfo
      tempPath = fileContent fileInfo

  -- Get file info
  fileSize <- getFileSize tempPath
  let mimeType = getMimeTypeFromExtension originalName
      config = defaultStorageConfig

  -- Use scheduled date if available, otherwise current time
  time <- maybe getCurrentTime pure mScheduledDate
  seed <- Random.getStdGen

  -- Validate upload
  case validateUpload AudioBucket originalName mimeType fileSize of
    Left err ->
      pure $ Left err
    Right () -> do
      -- Build upload result
      let uploadResult = buildEpisodeAudioUpload config showSlug episodeSlug originalName mimeType fileSize time seed
          storagePath = uploadResultStoragePath uploadResult

      -- Create directory structure
      createDirectoryIfMissing True (takeDirectory storagePath)

      -- Move file to final location
      copyFile tempPath storagePath
      pure $ Right uploadResult

-- | Handle episode artwork upload
uploadEpisodeArtwork ::
  (MonadIO m) =>
  Slug -> -- Show slug
  Slug -> -- Episode slug
  Maybe UTCTime -> -- Scheduled date (or Nothing to use current time)
  FileInfo FilePath -> -- Uploaded file info
  m (Either UploadError UploadResult)
uploadEpisodeArtwork showSlug episodeSlug mScheduledDate fileInfo = liftIO $ do
  let originalName = Text.decodeUtf8 $ fileName fileInfo
      tempPath = fileContent fileInfo

  -- Get file info
  fileSize <- getFileSize tempPath
  let mimeType = getMimeTypeFromExtension originalName
      config = defaultStorageConfig

  -- Use scheduled date if available, otherwise current time
  time <- maybe getCurrentTime pure mScheduledDate
  seed <- Random.getStdGen

  -- Validate upload
  case validateUpload ImageBucket originalName mimeType fileSize of
    Left err ->
      pure $ Left err
    Right () -> do
      -- Build upload result
      let uploadResult = buildEpisodeArtworkUpload config showSlug episodeSlug originalName mimeType fileSize time seed
          storagePath = uploadResultStoragePath uploadResult

      -- Create directory structure
      createDirectoryIfMissing True (takeDirectory storagePath)

      -- Move file to final location
      copyFile tempPath storagePath
      pure $ Right uploadResult

-- | Handle show logo upload
uploadShowLogo ::
  (MonadIO m) =>
  Slug ->
  FileInfo FilePath -> -- Uploaded file info
  m (Either UploadError UploadResult)
uploadShowLogo showSlug fileInfo = liftIO $ do
  let originalName = Text.decodeUtf8 $ fileName fileInfo
      tempPath = fileContent fileInfo

  -- Get file info
  fileSize <- getFileSize tempPath
  let mimeType = getMimeTypeFromExtension originalName
      config = defaultStorageConfig

  time <- getCurrentTime
  seed <- Random.getStdGen

  -- Validate upload
  case validateUpload ImageBucket originalName mimeType fileSize of
    Left err ->
      pure $ Left err
    Right () -> do
      -- Build upload result
      let uploadResult = buildShowLogoUpload config showSlug originalName mimeType fileSize time seed
          storagePath = uploadResultStoragePath uploadResult

      -- Create directory structure
      createDirectoryIfMissing True (takeDirectory storagePath)

      -- Move file to final location
      copyFile tempPath storagePath
      pure $ Right uploadResult

-- | Handle show banner upload
uploadShowBanner ::
  (MonadIO m) =>
  Slug ->
  FileInfo FilePath -> -- Uploaded file info
  m (Either UploadError UploadResult)
uploadShowBanner showSlug fileInfo = liftIO $ do
  let originalName = Text.decodeUtf8 $ fileName fileInfo
      tempPath = fileContent fileInfo

  -- Get file info
  fileSize <- getFileSize tempPath
  let mimeType = getMimeTypeFromExtension originalName
      config = defaultStorageConfig

  time <- getCurrentTime
  seed <- Random.getStdGen

  -- Validate upload
  case validateUpload ImageBucket originalName mimeType fileSize of
    Left err -> pure $ Left err
    Right () -> do
      -- Build upload result
      let uploadResult = buildShowBannerUpload config showSlug originalName mimeType fileSize time seed
          storagePath = uploadResultStoragePath uploadResult

      -- Create directory structure
      createDirectoryIfMissing True (takeDirectory storagePath)

      -- Move file to final location
      copyFile tempPath storagePath
      pure $ Right uploadResult

--------------------------------------------------------------------------------
-- Helper functions

getFileSize :: FilePath -> IO Int64
getFileSize path = do
  content <- BS.readFile path
  pure $ fromIntegral $ BS.length content

-- | Get MIME type from file extension (simple mapping)
getMimeTypeFromExtension :: Text -> Text
getMimeTypeFromExtension filename =
  let ext = Text.toLower $ Text.takeWhileEnd (/= '.') filename
   in case ext of
        "mp3" -> "audio/mpeg"
        "wav" -> "audio/wav"
        "flac" -> "audio/flac"
        "aac" -> "audio/aac"
        "ogg" -> "audio/ogg"
        "m4a" -> "audio/x-m4a"
        "jpg" -> "image/jpeg"
        "jpeg" -> "image/jpeg"
        "png" -> "image/png"
        "webp" -> "image/webp"
        "gif" -> "image/gif"
        _ -> "application/octet-stream"
