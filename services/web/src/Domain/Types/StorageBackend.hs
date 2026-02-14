module Domain.Types.StorageBackend
  ( -- * Storage Backend
    StorageBackend (..),
    LocalStorageConfig (..),
    S3StorageConfig (..),

    -- * Default Configurations
    defaultLocalConfig,

    -- * URL Construction
    buildMediaUrl,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)

--------------------------------------------------------------------------------

-- | Storage backend configuration for file uploads.
--
-- In Development mode, files are stored on the local filesystem.
-- In Production mode, files are uploaded to AWS S3.
data StorageBackend
  = LocalStorage LocalStorageConfig
  | S3Storage S3StorageConfig
  deriving stock (Show, Eq)

-- | Local filesystem storage configuration.
newtype LocalStorageConfig = LocalStorageConfig
  { -- | Root directory for file storage (e.g., "/tmp/kpbj")
    localStorageRoot :: FilePath
  }
  deriving stock (Show, Eq)

-- | AWS S3 storage configuration.
--
-- Supports both standard AWS S3 and S3-compatible services like DigitalOcean Spaces.
data S3StorageConfig = S3StorageConfig
  { -- | S3 bucket name (e.g., "kpbj-media" or "staging-kpbj-storage")
    s3BucketName :: Text,
    -- | AWS region (e.g., "us-west-2" or "sfo3" for DigitalOcean Spaces)
    s3Region :: Text,
    -- | Base URL for public access (e.g., "https://staging-kpbj-storage.sfo3.digitaloceanspaces.com")
    s3BaseUrl :: Text,
    -- | Custom S3 endpoint URL for S3-compatible services (e.g., "https://sfo3.digitaloceanspaces.com")
    -- When Nothing, uses standard AWS S3 endpoints.
    s3EndpointUrl :: Maybe Text
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- | Default local storage configuration for development.
defaultLocalConfig :: LocalStorageConfig
defaultLocalConfig =
  LocalStorageConfig
    { localStorageRoot = "/tmp/kpbj"
    }

--------------------------------------------------------------------------------

-- | Build a public URL for a stored file based on the storage backend.
--
-- For local storage, returns a path relative to the /media route.
-- For S3 storage, returns the full S3 URL.
--
-- Example:
-- >>> buildMediaUrl (LocalStorage defaultLocalConfig) "images/2024/01/01/logos/slug.jpg"
-- "/media/images/2024/01/01/logos/slug.jpg"
--
-- >>> buildMediaUrl (S3Storage config) "images/2024/01/01/logos/slug.jpg"
-- "https://bucket.s3.region.amazonaws.com/images/2024/01/01/logos/slug.jpg"
buildMediaUrl :: StorageBackend -> Text -> Text
buildMediaUrl backend objectKey = case backend of
  LocalStorage _ -> "/media/" <> objectKey
  S3Storage config -> s3BaseUrl config <> "/" <> objectKey
