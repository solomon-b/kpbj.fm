module Domain.Types.FileStorage where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Word (Word8)
import Domain.Types.Slug (Slug)
import System.FilePath ((</>))
import System.Random qualified as Random
import Text.Printf (printf)

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
buildStoragePath :: StorageConfig -> BucketType -> ResourceType -> DateHierarchy -> Text -> FilePath
buildStoragePath config bucketType resourceType dateHier filename =
  let rootPath = storageRoot config
      bucketPath = Text.unpack $ storageBucket config
      bucketTypePath' = Text.unpack $ bucketTypePath bucketType
      yearPath = Text.unpack $ dateYear dateHier
      monthPath = Text.unpack $ dateMonth dateHier
      dayPath = Text.unpack $ dateDay dateHier
      resourcePath = Text.unpack $ resourceTypePath resourceType
      filename' = Text.unpack filename
   in rootPath </> bucketPath </> bucketTypePath' </> yearPath </> monthPath </> dayPath </> resourcePath </> filename'

-- | Build URL path for serving files (without /tmp prefix)
-- Example: /static/kpbj/audio/2024/09/27/episodes/show-slug_episode-123_audio.mp3
buildUrlPath :: BucketType -> ResourceType -> DateHierarchy -> Slug -> Text -> Text
buildUrlPath bucketType resourceType dateHier showSlug filename =
  let parts =
        [ "static",
          "kpbj",
          bucketTypePath bucketType,
          dateYear dateHier,
          dateMonth dateHier,
          dateDay dateHier,
          resourceTypePath resourceType,
          display showSlug <> "_" <> filename
        ]
   in "/" <> Text.intercalate "/" parts

-- | Generate unique filename with timestamp
generateUniqueFilename :: Text -> Text -> Random.StdGen -> Text
generateUniqueFilename prefix extension seed =
  let hash = randomHash 16 seed
   in prefix <> "_" <> hash <> "." <> extension

randomHash :: Int -> Random.StdGen -> Text
randomHash len gen =
  let (bytes, _) = splitAt len $ Random.randoms gen :: ([Word8], [Word8])
      hash = concatMap (printf "%02x") bytes
   in Text.pack hash

--------------------------------------------------------------------------------
-- Common file path builders

-- | Build path for episode audio file
episodeAudioPath :: StorageConfig -> Slug -> UTCTime -> Random.StdGen -> FilePath
episodeAudioPath config episodeSlug time seed =
  let dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename (display episodeSlug) "mp3" seed
   in buildStoragePath config AudioBucket EpisodeAudio dateHier filename

-- | Build path for episode artwork
episodeArtworkPath :: StorageConfig -> Slug -> Slug -> UTCTime -> Random.StdGen -> FilePath
episodeArtworkPath config showSlug episodeSlug time seed =
  let dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename (display $ showSlug <> episodeSlug) "jpg" seed
   in buildStoragePath config ImageBucket EpisodeArtwork dateHier filename

-- | Build path for show logo
showLogoPath :: StorageConfig -> Slug -> UTCTime -> Random.StdGen -> FilePath
showLogoPath config showSlug time seed =
  let dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename (display showSlug) "png" seed
   in buildStoragePath config ImageBucket ShowLogo dateHier filename

-- | Build path for show banner
showBannerPath :: StorageConfig -> Slug -> UTCTime -> Random.StdGen -> FilePath
showBannerPath config showSlug time seed =
  let dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename (display showSlug) "jpg" seed
   in buildStoragePath config ImageBucket ShowBanner dateHier filename

-- | Build path for temporary upload
tempUploadPath :: StorageConfig -> Text -> UTCTime -> Random.StdGen -> FilePath
tempUploadPath config _originalFilename time seed =
  let dateHier = dateHierarchyFromTime time
      filename = generateUniqueFilename "temp" "tmp" seed
   in buildStoragePath config TempBucket TempUpload dateHier filename
