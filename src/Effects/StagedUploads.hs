{-# LANGUAGE QuasiQuotes #-}

-- | Shared operations for staged file uploads.
--
-- This module provides:
-- - Cryptographically secure token generation
-- - Upload claiming with authorization verification
-- - File cleanup utilities
module Effects.StagedUploads
  ( -- * Token Generation
    generateSecureToken,

    -- * Upload Claiming
    claimStagedUpload,
    claimStagedUploadMaybe,
    ClaimError (..),
    claimErrorToText,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------
-- Token Generation

-- | Generate a cryptographically secure token for staged uploads.
--
-- Uses UUID v4 (random) which is generated from a cryptographically secure
-- random number generator. The token is formatted as a 32-character hex string
-- (UUID without hyphens) for URL-safety and compact storage.
generateSecureToken :: (MonadIO m) => m StagedUploads.Token
generateSecureToken = liftIO $ do
  uuid <- UUID.V4.nextRandom
  -- Remove hyphens from UUID for cleaner tokens: "550e8400-e29b-..." -> "550e8400e29b..."
  let tokenText = Text.filter (/= '-') $ UUID.toText uuid
  pure $ StagedUploads.Token tokenText

--------------------------------------------------------------------------------
-- Upload Claiming

-- | Errors that can occur when claiming a staged upload.
data ClaimError
  = -- | Database query failed
    ClaimDbError Text
  | -- | Token not found, expired, or belongs to different user
    ClaimNotFound
  | -- | Upload type doesn't match expected type
    ClaimTypeMismatch StagedUploads.UploadType StagedUploads.UploadType
  deriving stock (Show, Eq)

-- | Claim a staged upload by token, returning the storage path.
--
-- Verifies:
-- - Token exists and is in 'pending' status
-- - Upload belongs to the specified user
-- - Upload type matches the expected type
--
-- On success, marks the upload as 'claimed' and returns the storage path.
claimStagedUpload ::
  ( MonadDB m,
    MonadReader env m,
    MonadUnliftIO m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    Log.MonadLog m
  ) =>
  User.Id ->
  Text ->
  StagedUploads.UploadType ->
  m (Either ClaimError Text)
claimStagedUpload userId tokenText expectedType = do
  let token = StagedUploads.Token tokenText
  result <- execQuerySpan (StagedUploads.claimUpload token userId)
  case result of
    Left err -> do
      Log.logInfo "Failed to claim staged upload" (Text.pack $ show err)
      pure $ Left $ ClaimDbError "Failed to claim uploaded file"
    Right Nothing -> do
      Log.logInfo "Staged upload not found or not authorized" tokenText
      pure $ Left ClaimNotFound
    Right (Just stagedUpload) -> do
      -- Verify the upload type matches
      let actualType = StagedUploads.uploadType stagedUpload
      if actualType /= expectedType
        then do
          Log.logInfo "Staged upload type mismatch" (Text.pack $ show (actualType, expectedType))
          pure $ Left $ ClaimTypeMismatch actualType expectedType
        else do
          Log.logInfo "Claimed staged upload" (StagedUploads.storagePath stagedUpload)
          pure $ Right $ StagedUploads.storagePath stagedUpload

-- | Convert a ClaimError to user-friendly text.
claimErrorToText :: ClaimError -> Text
claimErrorToText = \case
  ClaimDbError msg -> msg
  ClaimNotFound -> "Uploaded file not found or expired"
  ClaimTypeMismatch _ _ -> "Uploaded file type mismatch"

-- | Claim a staged upload, returning @Either Text (Maybe Text)@.
--
-- This is a convenience wrapper around 'claimStagedUpload' that converts
-- the error type and wraps the result in Maybe for compatibility with
-- file upload handlers that accept both staged and direct uploads.
claimStagedUploadMaybe ::
  ( MonadDB m,
    MonadReader env m,
    MonadUnliftIO m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    Log.MonadLog m
  ) =>
  User.Id ->
  Text ->
  StagedUploads.UploadType ->
  m (Either Text (Maybe Text))
claimStagedUploadMaybe userId tokenText expectedType = do
  result <- claimStagedUpload userId tokenText expectedType
  pure $ case result of
    Left err -> Left $ claimErrorToText err
    Right path -> Right $ Just path
