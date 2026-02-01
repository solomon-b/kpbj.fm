module Domain.Types.FileStorage where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.LocalTime (localDay)
import Data.Word (Word8)
import Domain.Types.Timezone (utcToPacific)
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
  | BlogHeroImage
  | EventPosterImage
  | UserAvatar
  | StationIdAudio
  | EphemeralAudio
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
  BlogHeroImage -> "blog-heroes"
  EventPosterImage -> "event-posters"
  UserAvatar -> "avatars"
  StationIdAudio -> "station-ids"
  EphemeralAudio -> "ephemeral"
  TempUpload -> "uploads"

-- | Create date hierarchy from UTCTime.
--
-- Converts to Pacific time before extracting the date, so file paths
-- are organized by the local air date (what users see on the site)
-- rather than the UTC date.
dateHierarchyFromTime :: UTCTime -> DateHierarchy
dateHierarchyFromTime time =
  let pacificDay = localDay (utcToPacific time)
      dayStr = formatTime defaultTimeLocale "%F" pacificDay -- YYYY-MM-DD
      parts = Text.splitOn "-" (Text.pack dayStr)
   in case parts of
        [year, month, dayOfMonth] -> DateHierarchy year month dayOfMonth
        _ -> DateHierarchy "1970" "01" "01" -- Fallback date

-- | Build storage key (object key) for S3 or relative path.
--
-- This key is environment-independent and used as the path in S3 or
-- the relative path for local storage. It is what gets stored in the database.
--
-- Structure: {bucket}/{resource}/{year}/{month}/{day}/{filename}
-- Example: audio/episodes/2024/09/27/show-slug_2024-09-27_abc123.mp3
buildStorageKey :: BucketType -> ResourceType -> DateHierarchy -> Text -> Text
buildStorageKey bucketType resourceType dateHier filename =
  Text.intercalate
    "/"
    [ bucketTypePath bucketType,
      resourceTypePath resourceType,
      dateYear dateHier,
      dateMonth dateHier,
      dateDay dateHier,
      filename
    ]

-- | Build a flat storage key without date hierarchy.
--
-- Used for staging area where files await claiming. No date organization
-- since files will be moved to their final location with the correct date
-- when claimed.
--
-- Example: temp/staging/abc123def456.mp3
buildStagingKey :: BucketType -> Text -> Text -> Text
buildStagingKey bucketType subdir filename =
  Text.intercalate
    "/"
    [ bucketTypePath bucketType,
      subdir,
      filename
    ]

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
