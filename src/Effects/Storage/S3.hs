{-# LANGUAGE ScopedTypeVariables #-}

module Effects.Storage.S3
  ( -- * S3 Storage Operations
    storeFileS3,

    -- * Error Types
    S3UploadError (..),
  )
where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.PutObject (putObject_contentType)
import Control.Exception (IOException, SomeException)
import Control.Exception qualified as Exception
import Control.Lens ((&), (?~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.FileStorage (BucketType, DateHierarchy, ResourceType, buildStorageKey)
import Domain.Types.StorageBackend (S3StorageConfig (..))

--------------------------------------------------------------------------------

-- | Specific error types for S3 upload operations.
--
-- Provides more useful error information than catching 'SomeException'.
data S3UploadError
  = -- | AWS service error (auth, permissions, bucket issues)
    -- TODO: Factor out record into its own type.
    S3ServiceError
      { s3ErrorCode :: Text,
        s3ErrorMessage :: Text,
        s3StatusCode :: Int
      }
  | -- | Network error (connection, timeout)
    S3NetworkError Text
  | -- | Unexpected error
    S3UnexpectedError Text
  deriving stock (Show, Eq)

-- | Format an S3 upload error for display.
formatS3Error :: S3UploadError -> Text
formatS3Error = \case
  S3ServiceError code msg status ->
    "S3 service error (HTTP " <> Text.pack (show status) <> "): "
      <> code
      <> " - "
      <> msg
  S3NetworkError msg ->
    "S3 network error: " <> msg
  S3UnexpectedError msg ->
    "S3 unexpected error: " <> msg

--------------------------------------------------------------------------------

-- | Store a file to AWS S3.
--
-- Uploads the file content to the configured S3 bucket.
-- Returns the object key that can be used with 'buildMediaUrl'.
storeFileS3 ::
  (MonadIO m) =>
  AWS.Env ->
  S3StorageConfig ->
  BucketType ->
  ResourceType ->
  DateHierarchy ->
  -- | Filename
  Text ->
  -- | File content
  BS.ByteString ->
  -- | MIME type
  Text ->
  m (Either Text Text)
storeFileS3 awsEnv config bucketType resourceType dateHier filename content mimeType = liftIO $ do
  let objectKey = buildStorageKey bucketType resourceType dateHier filename
      bucket = S3.BucketName (s3BucketName config)
      key = S3.ObjectKey objectKey
      body = AWS.toBody content

  result <- Exception.try $ runResourceT $ do
    let req =
          S3.newPutObject bucket key body
            & putObject_contentType ?~ mimeType
    AWS.send awsEnv req

  case result of
    Right _ -> pure $ Right objectKey
    Left err -> pure $ Left $ formatS3Error $ classifyS3Error err

-- | Classify an exception into a specific S3 error type.
classifyS3Error :: SomeException -> S3UploadError
classifyS3Error err
  -- Try to extract AWS service error
  | Just (serviceErr :: AWS.Error) <- Exception.fromException err =
      case serviceErr of
        AWS.ServiceError svcErr ->
          let msgText = case svcErr.message of
                Just (AWS.ErrorMessage msg) -> msg
                Nothing -> "No error message"
           in S3ServiceError
                { s3ErrorCode = Text.pack $ show svcErr.code,
                  s3ErrorMessage = msgText,
                  s3StatusCode = fromIntegral $ fromEnum svcErr.status
                }
        AWS.TransportError httpErr ->
          S3NetworkError $ "HTTP transport error: " <> Text.pack (show httpErr)
        AWS.SerializeError serErr ->
          S3UnexpectedError $ "Serialization error: " <> Text.pack (show serErr)
  -- Network/IO errors
  | Just (ioErr :: IOException) <- Exception.fromException err =
      S3NetworkError $ Text.pack (show ioErr)
  -- Fallback
  | otherwise =
      S3UnexpectedError $ Text.pack (show err)
