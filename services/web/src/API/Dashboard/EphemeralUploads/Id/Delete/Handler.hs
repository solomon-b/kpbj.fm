{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.EphemeralUploads.Id.Delete.Handler (handler, action) where

--------------------------------------------------------------------------------

import Amazonka qualified as AWS
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.StagedUploadCleanup (deleteFile)
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue running action and returning banner response.
handler ::
  EphemeralUploads.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler ephemeralUploadId cookie =
  handleBannerErrors "Ephemeral upload delete" $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to delete ephemeral uploads." userMetadata
    action user userMetadata ephemeralUploadId
    pure $ do
      mempty -- Empty response removes the target element
      renderBanner Success "Ephemeral Deleted" "The ephemeral upload has been deleted successfully."

-- | Business logic: fetch, authorize, delete file and record.
action ::
  User.Model ->
  UserMetadata.Model ->
  EphemeralUploads.Id ->
  ExceptT HandlerError AppM ()
action user userMetadata ephemeralUploadId = do
  -- 1. Fetch ephemeral upload
  ephemeralUpload <- fetchEphemeralUpload ephemeralUploadId

  -- 2. Check authorization: must be creator OR staff/admin
  let isCreator = ephemeralUpload.eumCreatorId == user.mId
      isStaffOrAdmin = UserMetadata.isStaffOrHigher userMetadata.mUserRole
      isAuthorized = isCreator || isStaffOrAdmin

  unless isAuthorized $
    throwNotAuthorized "You can only delete ephemeral uploads you created." (Just userMetadata.mUserRole)

  -- 3. Delete the ephemeral upload
  deleteEphemeralUpload ephemeralUpload

--------------------------------------------------------------------------------
-- Helpers

fetchEphemeralUpload ::
  EphemeralUploads.Id ->
  ExceptT HandlerError AppM EphemeralUploads.Model
fetchEphemeralUpload ephemeralUploadId =
  fromMaybeM (throwNotFound "Ephemeral upload") $
    fromRightM throwDatabaseError $
      execQuery (EphemeralUploads.getEphemeralUploadById ephemeralUploadId)

deleteEphemeralUpload ::
  EphemeralUploads.Model ->
  ExceptT HandlerError AppM ()
deleteEphemeralUpload ephemeralUpload = do
  -- 1. Get storage backend and AWS env for file deletion
  backend <- asks (Has.getter @StorageBackend)
  mAwsEnv <- asks (Has.getter @(Maybe AWS.Env))

  -- 2. Delete file from storage (best effort - continue even if file deletion fails)
  fileDeleted <- deleteFile backend mAwsEnv ephemeralUpload.eumAudioFilePath
  unless fileDeleted $
    Log.logInfo "Failed to delete ephemeral upload file from storage" ephemeralUpload.eumAudioFilePath

  -- 3. Delete database record
  execQuery (EphemeralUploads.deleteEphemeralUpload ephemeralUpload.eumId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Ephemeral upload"
    Right (Just _) ->
      Log.logInfo "Ephemeral upload deleted successfully" ephemeralUpload.eumId
