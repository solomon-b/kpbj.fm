module Domain.Types.FileStorage where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Domain.Types.Slug (Slug)
import System.FilePath ((</>))

--------------------------------------------------------------------------------

-- | File storage structure mimicking AWS S3 bucket organization
-- Root structure: /tmp/kpbj/{bucket_type}/{date_hierarchy}/{resource_type}/
data StorageConfig = StorageConfig
  { storageRoot :: FilePath, -- "/tmp"
    storageBucket :: Text -- "kpbj"
  }
  deriving (Show, Eq)

-- | Different content buckets (like S3 buckets)
data BucketType
  = AudioBucket -- Audio files for episodes
  | ImageBucket -- Images, artwork, logos
  | DocumentBucket -- Documents, PDFs
  | TempBucket -- Temporary uploads before processing
  deriving (Show, Eq)

-- | Resource types for organization
data ResourceType
  = EpisodeAudio
  | EpisodeArtwork
  | ShowLogo
  | ShowBanner
  | TempUpload
  deriving (Show, Eq)

-- | Date-based hierarchy for organization
data DateHierarchy = DateHierarchy
  { dateYear :: Text,
    dateMonth :: Text,
    dateDay :: Text
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

defaultStorageConfig :: StorageConfig
defaultStorageConfig =
  StorageConfig
    { storageRoot = "/tmp",
      storageBucket = "kpbj"
    }

bucketTypePath :: BucketType -> Text
bucketTypePath = \case
  AudioBucket -> "audio"
  ImageBucket -> "images"
  DocumentBucket -> "documents"
  TempBucket -> "temp"

resourceTypePath :: ResourceType -> Text
resourceTypePath = \case
  EpisodeAudio -> "episodes"
  EpisodeArtwork -> "artwork"
  ShowLogo -> "logos"
  ShowBanner -> "banners"
  TempUpload -> "uploads"

-- | Create date hierarchy from UTCTime
dateHierarchyFromTime :: UTCTime -> DateHierarchy
dateHierarchyFromTime time =
  let day = formatTime defaultTimeLocale "%F" time -- YYYY-MM-DD
      parts = Text.splitOn "-" (Text.pack day)
   in case parts of
        [year, month, dayOfMonth] -> DateHierarchy year month dayOfMonth
        _ -> DateHierarchy "1970" "01" "01" -- Fallback date

-- | Build full storage path
-- Example: /tmp/kpbj/audio/2024/09/27/episodes/show-slug_episode-123_audio.mp3
buildStoragePath :: StorageConfig -> BucketType -> ResourceType -> DateHierarchy -> Slug -> Text -> FilePath
buildStoragePath config bucketType resourceType dateHier showSlug filename =
  let rootPath = storageRoot config
      bucketPath = Text.unpack $ storageBucket config
      bucketTypePath' = Text.unpack $ bucketTypePath bucketType
      yearPath = Text.unpack $ dateYear dateHier
      monthPath = Text.unpack $ dateMonth dateHier
      dayPath = Text.unpack $ dateDay dateHier
      resourcePath = Text.unpack $ resourceTypePath resourceType
      prefixedFilename = Text.unpack $ display showSlug <> "_" <> filename
   in rootPath </> bucketPath </> bucketTypePath' </> yearPath </> monthPath </> dayPath </> resourcePath </> prefixedFilename

-- | Build URL path for serving files (without /tmp prefix)
-- Example: /static/kpbj/audio/2024/09/27/episodes/show-slug_episode-123_audio.mp3
buildUrlPath :: BucketType -> ResourceType -> DateHierarchy -> Text -> Text -> Text
buildUrlPath bucketType resourceType dateHier showSlug filename =
  let parts =
        [ "static",
          "kpbj",
          bucketTypePath bucketType,
          dateYear dateHier,
          dateMonth dateHier,
          dateDay dateHier,
          resourceTypePath resourceType,
          showSlug <> "_" <> filename
        ]
   in "/" <> Text.intercalate "/" parts

-- | Generate unique filename with timestamp
generateUniqueFilename :: Text -> Text -> UTCTime -> Text
generateUniqueFilename prefix extension time =
  let timestamp = Text.pack $ formatTime defaultTimeLocale "%Y%m%d_%H%M%S" time
   in prefix <> "_" <> timestamp <> "." <> extension

--------------------------------------------------------------------------------

-- Common file path builders

-- | Build path for episode audio file
episodeAudioPath :: StorageConfig -> Slug -> Slug -> UTCTime -> FilePath
episodeAudioPath config showSlug episodeSlug time =
  let dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename (display episodeSlug) "mp3" time
   in buildStoragePath config AudioBucket EpisodeAudio dateHier showSlug filename

-- | Build path for episode artwork
episodeArtworkPath :: StorageConfig -> Slug -> Slug -> UTCTime -> FilePath
episodeArtworkPath config showSlug episodeSlug time =
  let dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename (display episodeSlug) "jpg" time
   in buildStoragePath config ImageBucket EpisodeArtwork dateHier showSlug filename

-- | Build path for show logo
showLogoPath :: StorageConfig -> Slug -> UTCTime -> FilePath
showLogoPath config showSlug time =
  let dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename "logo" "png" time
   in buildStoragePath config ImageBucket ShowLogo dateHier showSlug filename

-- | Build path for show banner
showBannerPath :: StorageConfig -> Slug -> UTCTime -> FilePath
showBannerPath config showSlug time =
  let dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename "banner" "jpg" time
   in buildStoragePath config ImageBucket ShowBanner dateHier showSlug filename

-- | Build path for temporary upload
tempUploadPath :: StorageConfig -> Text -> UTCTime -> FilePath
tempUploadPath config _originalFilename time =
  let dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename "temp" "tmp" time
   in buildStoragePath config TempBucket TempUpload dateHier "temp" filename
