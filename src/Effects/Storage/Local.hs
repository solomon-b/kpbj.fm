module Effects.Storage.Local
  ( -- * Local Storage Operations
    storeFileLocal,
    buildLocalPath,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.FileStorage (BucketType, DateHierarchy (..), ResourceType, bucketTypePath, buildStorageKey, resourceTypePath)
import Domain.Types.StorageBackend (LocalStorageConfig (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

--------------------------------------------------------------------------------

-- | Store a file to the local filesystem.
--
-- Creates the necessary directory structure and writes the file content.
-- Returns the object key (relative path) that can be used with 'buildMediaUrl'.
storeFileLocal ::
  (MonadIO m) =>
  LocalStorageConfig ->
  BucketType ->
  ResourceType ->
  DateHierarchy ->
  -- | Filename
  Text ->
  -- | File content
  BS.ByteString ->
  m (Either Text Text)
storeFileLocal config bucketType resourceType dateHier filename content = liftIO $ do
  let fullPath = buildLocalPath config bucketType resourceType dateHier filename
      objectKey = buildStorageKey bucketType resourceType dateHier filename

  -- Create directory structure
  createDirectoryIfMissing True (takeDirectory fullPath)

  -- Write file
  BS.writeFile fullPath content

  pure $ Right objectKey

-- | Build the full local filesystem path for a file.
--
-- Example: /tmp/kpbj/images/2024/01/15/logos/show-slug_abc123.png
buildLocalPath ::
  LocalStorageConfig ->
  BucketType ->
  ResourceType ->
  DateHierarchy ->
  -- | Filename
  Text ->
  FilePath
buildLocalPath config bucketType resourceType dateHier filename =
  localStorageRoot config
    </> Text.unpack (bucketTypePath bucketType)
    </> Text.unpack (dateYear dateHier)
    </> Text.unpack (dateMonth dateHier)
    </> Text.unpack (dateDay dateHier)
    </> Text.unpack (resourceTypePath resourceType)
    </> Text.unpack filename
