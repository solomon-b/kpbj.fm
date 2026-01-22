{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.EphemeralUploads.Id.Delete.Handler (handler) where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.StagedUploadCleanup (deleteFile)
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  EphemeralUploads.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler _tracer ephemeralUploadId cookie =
  handleBannerErrors "Ephemeral upload delete" $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to delete ephemeral uploads." userMetadata

    -- 2. Fetch ephemeral upload
    ephemeralUpload <- fetchEphemeralUpload ephemeralUploadId

    -- 3. Check authorization: must be creator OR staff/admin
    let isCreator = ephemeralUpload.eumCreatorId == user.mId
        isStaffOrAdmin = UserMetadata.isStaffOrHigher userMetadata.mUserRole
        isAuthorized = isCreator || isStaffOrAdmin

    unless isAuthorized $
      throwNotAuthorized "You can only delete ephemeral uploads you created." (Just userMetadata.mUserRole)

    -- 4. Delete the ephemeral upload
    deleteEphemeralUpload ephemeralUpload

--------------------------------------------------------------------------------
-- Helpers

fetchEphemeralUpload ::
  EphemeralUploads.Id ->
  AppM EphemeralUploads.Model
fetchEphemeralUpload ephemeralUploadId =
  execQuerySpan (EphemeralUploads.getEphemeralUploadById ephemeralUploadId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Ephemeral upload"
    Right (Just ephemeralUpload) -> pure ephemeralUpload

deleteEphemeralUpload ::
  EphemeralUploads.Model ->
  AppM (Lucid.Html ())
deleteEphemeralUpload ephemeralUpload = do
  -- 1. Get storage backend and AWS env for file deletion
  backend <- asks (Has.getter @StorageBackend)
  mAwsEnv <- asks (Has.getter @(Maybe AWS.Env))

  -- 2. Delete file from storage (best effort - continue even if file deletion fails)
  fileDeleted <- deleteFile backend mAwsEnv ephemeralUpload.eumAudioFilePath
  unless fileDeleted $
    Log.logInfo "Failed to delete ephemeral upload file from storage" ephemeralUpload.eumAudioFilePath

  -- 3. Delete database record
  execQuerySpan (EphemeralUploads.deleteEphemeralUpload ephemeralUpload.eumId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Ephemeral upload"
    Right (Just _) -> do
      Log.logInfo "Ephemeral upload deleted successfully" ephemeralUpload.eumId
      -- Return empty (removes row) + OOB banner (Pattern C)
      pure $ do
        mempty -- Empty response removes the target element
        renderBanner Success "Ephemeral Deleted" "The ephemeral upload has been deleted successfully."
