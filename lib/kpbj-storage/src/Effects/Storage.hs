-- | Storage operations that pick the backend.
module Effects.Storage
  ( deleteFile,
  )
where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Domain.Types.StorageBackend (StorageBackend (..))
import Effects.Storage.Local qualified as Local
import Effects.Storage.S3 qualified as S3

--------------------------------------------------------------------------------

-- | Delete a file from the configured storage backend.
--
-- A file that does not exist counts as success. S3 storage with no AWS
-- environment is an error.
deleteFile ::
  (MonadIO m) =>
  StorageBackend ->
  Maybe AWS.Env ->
  -- | Storage path (object key)
  Text ->
  m (Either Text ())
deleteFile backend mAwsEnv storagePath =
  case backend of
    LocalStorage config -> Local.deleteFileLocal config storagePath
    S3Storage config -> case mAwsEnv of
      Nothing -> pure $ Left "S3 storage has no AWS environment"
      Just awsEnv -> S3.deleteFileS3 awsEnv config storagePath
