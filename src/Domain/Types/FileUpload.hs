module Domain.Types.FileUpload where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Domain.Types.FileStorage
import Domain.Types.Slug (Slug)
import System.Random qualified as Random

--------------------------------------------------------------------------------

data UploadResult = UploadResult
  { uploadResultOriginalName :: Text,
    uploadResultStoragePath :: FilePath,
    uploadResultMimeType :: Text,
    uploadResultFileSize :: Int64
  }
  deriving (Show, Eq)

data UploadError
  = UnsupportedFileType Text
  | FileTooLarge Int64 Int64 -- actual size, max size
  | InvalidFileName Text
  | StorageError Text
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- File validation

isAudioFile :: Text -> Bool
isAudioFile mimeType = mimeType `elem` allowedAudioTypes

isImageFile :: Text -> Bool
isImageFile mimeType = mimeType `elem` allowedImageTypes

allowedAudioTypes :: [Text]
allowedAudioTypes =
  [ "audio/mpeg",
    "audio/wav",
    "audio/flac",
    "audio/aac",
    "audio/ogg",
    "audio/x-m4a"
  ]

allowedImageTypes :: [Text]
allowedImageTypes =
  [ "image/jpeg",
    "image/jpg",
    "image/png",
    "image/webp",
    "image/gif"
  ]

-- | Maximum audio file size in bytes: 500MB
maxAudioFileSize :: Int64
maxAudioFileSize = 500 * 1024 * 1024

-- | Maximum image file size in bytes: 10MB
maxImageFileSize :: Int64
maxImageFileSize = 10 * 1024 * 1024

--------------------------------------------------------------------------------
-- Upload validation

validateUpload :: BucketType -> Text -> Text -> Int64 -> Either UploadError ()
validateUpload bucketType filename mimeType fileSize = do
  validateFileName filename
  validateFileType bucketType mimeType
  validateFileSize bucketType fileSize

validateFileName :: Text -> Either UploadError ()
validateFileName filename
  | Text.null filename = Left $ InvalidFileName "Empty filename"
  | Text.length filename > 255 = Left $ InvalidFileName "Filename too long"
  | Text.any (`elem` ['/', '\\', ':', '*', '?', '"', '<', '>', '|']) filename =
      Left $ InvalidFileName "Filename contains invalid characters"
  | otherwise = Right ()

validateFileType :: BucketType -> Text -> Either UploadError ()
validateFileType bucketType mimeType = case bucketType of
  AudioBucket ->
    if isAudioFile mimeType
      then Right ()
      else Left $ UnsupportedFileType $ "Audio files only. Got: " <> mimeType
  ImageBucket ->
    if isImageFile mimeType
      then Right ()
      else Left $ UnsupportedFileType $ "Image files only. Got: " <> mimeType
  _ -> Right () -- Other buckets allow any file type

validateFileSize :: BucketType -> Int64 -> Either UploadError ()
validateFileSize bucketType fileSize = case bucketType of
  AudioBucket ->
    if fileSize <= maxAudioFileSize
      then Right ()
      else Left $ FileTooLarge fileSize maxAudioFileSize
  ImageBucket ->
    if fileSize <= maxImageFileSize
      then Right ()
      else Left $ FileTooLarge fileSize maxImageFileSize
  _ -> Right () -- Other buckets have no size limit

--------------------------------------------------------------------------------
-- Upload result builders

buildEpisodeAudioUpload :: StorageConfig -> Slug -> Slug -> Text -> Text -> Int64 -> UTCTime -> Random.StdGen -> UploadResult
buildEpisodeAudioUpload config showSlug episodeSlug originalName mimeType fileSize time seed =
  let baseFilename = showSlug <> episodeSlug
      storagePath = episodeAudioPath config baseFilename time seed
   in UploadResult
        { uploadResultOriginalName = originalName,
          uploadResultStoragePath = storagePath,
          uploadResultMimeType = mimeType,
          uploadResultFileSize = fileSize
        }

buildEpisodeArtworkUpload :: StorageConfig -> Slug -> Slug -> Text -> Text -> Int64 -> UTCTime -> Random.StdGen -> UploadResult
buildEpisodeArtworkUpload config showSlug episodeSlug originalName mimeType fileSize time seed =
  UploadResult
    { uploadResultOriginalName = originalName,
      uploadResultStoragePath = episodeArtworkPath config showSlug episodeSlug time seed,
      uploadResultMimeType = mimeType,
      uploadResultFileSize = fileSize
    }

buildShowLogoUpload :: StorageConfig -> Slug -> Text -> Text -> Int64 -> UTCTime -> Random.StdGen -> UploadResult
buildShowLogoUpload config showSlug originalName mimeType fileSize time seed =
  UploadResult
    { uploadResultOriginalName = originalName,
      uploadResultStoragePath = showLogoPath config showSlug time seed,
      uploadResultMimeType = mimeType,
      uploadResultFileSize = fileSize
    }

buildShowBannerUpload :: StorageConfig -> Slug -> Text -> Text -> Int64 -> UTCTime -> Random.StdGen -> UploadResult
buildShowBannerUpload config showSlug originalName mimeType fileSize time seed =
  UploadResult
    { uploadResultOriginalName = originalName,
      uploadResultStoragePath = showBannerPath config showSlug time seed,
      uploadResultMimeType = mimeType,
      uploadResultFileSize = fileSize
    }
