module Effects.FileUpload where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (hPut)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (getCurrentTime)
import Domain.Types.FileStorage
import Domain.Types.FileUpload
import Network.Wai.Parse (FileInfo (..))
import System.Directory (copyFile, createDirectoryIfMissing, removeFile)
import System.FilePath (takeDirectory)
import System.IO.Temp (withSystemTempFile)

--------------------------------------------------------------------------------

-- | Handle file upload with proper validation and storage
uploadEpisodeAudio ::
  (MonadIO m) =>
  Text -> -- Show slug
  FileInfo FilePath -> -- Uploaded file info
  m (Either UploadError UploadResult)
uploadEpisodeAudio showSlug fileInfo = liftIO $ do
  let originalName = Text.decodeUtf8 $ fileName fileInfo
      tempPath = fileContent fileInfo

  -- Get file info
  fileSize <- getFileSize tempPath
  let mimeType = getMimeTypeFromExtension originalName
      config = defaultStorageConfig

  time <- getCurrentTime

  -- Validate upload
  case validateUpload AudioBucket originalName mimeType fileSize of
    Left err -> pure $ Left err
    Right () -> do
      -- Build upload result
      let uploadResult = buildEpisodeAudioUpload config showSlug originalName mimeType fileSize time
          storagePath = uploadResultStoragePath uploadResult

      -- Create directory structure
      createDirectoryIfMissing True (takeDirectory storagePath)

      -- Move file to final location
      copyFile tempPath storagePath
      pure $ Right uploadResult

-- | Handle episode artwork upload
uploadEpisodeArtwork ::
  (MonadIO m) =>
  Text -> -- Show slug
  FileInfo FilePath -> -- Uploaded file info
  m (Either UploadError UploadResult)
uploadEpisodeArtwork showSlug fileInfo = liftIO $ do
  let originalName = Text.decodeUtf8 $ fileName fileInfo
      tempPath = fileContent fileInfo

  -- Get file info
  fileSize <- getFileSize tempPath
  let mimeType = getMimeTypeFromExtension originalName
      config = defaultStorageConfig

  time <- getCurrentTime

  -- Validate upload
  case validateUpload ImageBucket originalName mimeType fileSize of
    Left err -> pure $ Left err
    Right () -> do
      -- Build upload result
      let uploadResult = buildEpisodeArtworkUpload config showSlug originalName mimeType fileSize time
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

-- | Create a temporary file for processing uploads
withTempUpload :: (MonadIO m) => BS.ByteString -> (FilePath -> IO a) -> m a
withTempUpload content action = liftIO $
  withSystemTempFile "kpbj-upload" $ \tempPath tempHandle -> do
    hPut tempHandle content
    action tempPath

-- | Process multiple file uploads (for batch operations)
processMultipleUploads ::
  (MonadIO m) =>
  Text -> -- Show slug
  [FileInfo FilePath] -> -- List of files
  m [Either UploadError UploadResult]
processMultipleUploads showSlug files = do
  mapM processFile files
  where
    processFile fileInfo = do
      let originalName = Text.decodeUtf8 $ fileName fileInfo
          mimeType = getMimeTypeFromExtension originalName

      -- Route to appropriate handler based on file type
      if isAudioFile mimeType
        then uploadEpisodeAudio showSlug fileInfo
        else
          if isImageFile mimeType
            then uploadEpisodeArtwork showSlug fileInfo
            else pure $ Left $ UnsupportedFileType $ "Unsupported file type: " <> mimeType

--------------------------------------------------------------------------------
-- Cleanup functions

-- | Clean up temporary files after processing
cleanupTempFiles :: (MonadIO m) => [FilePath] -> m ()
cleanupTempFiles paths =
  liftIO $
    mapM_ removeFile paths

-- | Clean up old uploaded files (for maintenance)
cleanupOldUploads :: (MonadIO m) => Int -> m () -- Days to keep
cleanupOldUploads _daysToKeep = liftIO $ do
  -- TODO: Implement cleanup of old files in /tmp/kpbj
  -- This would scan for files older than X days and remove them
  -- Left as TODO for now since it requires more complex file system operations
  pure ()
