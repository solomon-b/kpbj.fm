module Domain.Types.FileUpload where

--------------------------------------------------------------------------------

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Domain.Types.FileStorage
import Domain.Types.Slug (Slug)
import System.FilePath (takeBaseName)

--------------------------------------------------------------------------------

data UploadResult = UploadResult
  { uploadResultOriginalName :: Text,
    uploadResultStoragePath :: FilePath,
    uploadResultUrl :: Text,
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
  [ "audio/mpeg", -- MP3
    "audio/wav", -- WAV
    "audio/flac", -- FLAC
    "audio/aac", -- AAC
    "audio/ogg", -- OGG
    "audio/x-m4a" -- M4A
  ]

allowedImageTypes :: [Text]
allowedImageTypes =
  [ "image/jpeg", -- JPEG
    "image/jpg", -- JPG
    "image/png", -- PNG
    "image/webp", -- WebP
    "image/gif" -- GIF
  ]

-- Maximum file sizes in bytes
maxAudioFileSize :: Int64
maxAudioFileSize = 500 * 1024 * 1024 -- 500MB

maxImageFileSize :: Int64
maxImageFileSize = 10 * 1024 * 1024 -- 10MB

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
-- File extension mapping

getFileExtension :: Text -> Text
getFileExtension mimeType = case mimeType of
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
  _ -> "bin" -- Default extension for unknown types

-- Extract clean filename without extension for prefixing
getCleanFilename :: Text -> Text
getCleanFilename filename =
  let withoutExt = Text.pack $ takeBaseName $ Text.unpack filename
      cleaned = Text.filter (\c -> c /= ' ' && c /= '-' && isAlphaNumeric c) withoutExt
   in if Text.null cleaned then "file" else cleaned
  where
    isAlphaNumeric c = isAsciiLower c || isAsciiUpper c || isDigit c

--------------------------------------------------------------------------------
-- Upload result builders

buildEpisodeAudioUpload :: StorageConfig -> Slug -> Slug -> Text -> Text -> Int64 -> UTCTime -> UploadResult
buildEpisodeAudioUpload config showSlug episodeSlug originalName mimeType fileSize time =
  let extension = getFileExtension mimeType
      baseFilename = showSlug <> episodeSlug
      storagePath = episodeAudioPath config showSlug baseFilename time
      dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename (display baseFilename) extension time
      url = buildUrlPath AudioBucket EpisodeAudio dateHier (display showSlug) filename
   in UploadResult
        { uploadResultOriginalName = originalName,
          uploadResultStoragePath = storagePath,
          uploadResultUrl = url,
          uploadResultMimeType = mimeType,
          uploadResultFileSize = fileSize
        }

buildEpisodeArtworkUpload :: StorageConfig -> Slug -> Slug -> Text -> Text -> Int64 -> UTCTime -> UploadResult
buildEpisodeArtworkUpload config showSlug episodeSlug originalName mimeType fileSize time =
  let extension = getFileExtension mimeType
      baseFilename = showSlug <> episodeSlug
      storagePath = episodeArtworkPath config showSlug baseFilename time
      dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename (display baseFilename) extension time
      url = buildUrlPath ImageBucket EpisodeArtwork dateHier (display showSlug) filename
   in UploadResult
        { uploadResultOriginalName = originalName,
          uploadResultStoragePath = storagePath,
          uploadResultUrl = url,
          uploadResultMimeType = mimeType,
          uploadResultFileSize = fileSize
        }

buildShowLogoUpload :: StorageConfig -> Slug -> Text -> Text -> Int64 -> UTCTime -> UploadResult
buildShowLogoUpload config showSlug originalName mimeType fileSize time =
  let extension = getFileExtension mimeType
      storagePath = showLogoPath config showSlug time
      dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename "logo" extension time
      url = buildUrlPath ImageBucket ShowLogo dateHier (display showSlug) filename
   in UploadResult
        { uploadResultOriginalName = originalName,
          uploadResultStoragePath = storagePath,
          uploadResultUrl = url,
          uploadResultMimeType = mimeType,
          uploadResultFileSize = fileSize
        }

buildShowBannerUpload :: StorageConfig -> Slug -> Text -> Text -> Int64 -> UTCTime -> UploadResult
buildShowBannerUpload config showSlug originalName mimeType fileSize time =
  let extension = getFileExtension mimeType
      storagePath = showBannerPath config showSlug time
      dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename "banner" extension time
      url = buildUrlPath ImageBucket ShowBanner dateHier (display showSlug) filename
   in UploadResult
        { uploadResultOriginalName = originalName,
          uploadResultStoragePath = storagePath,
          uploadResultUrl = url,
          uploadResultMimeType = mimeType,
          uploadResultFileSize = fileSize
        }
