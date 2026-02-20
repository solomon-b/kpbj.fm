{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.EphemeralUploads.Id.Edit.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Id.Edit.Post.Route (FormData (..))
import API.Links (dashboardEphemeralUploadsLinks, rootLink)
import API.Types
import Amazonka qualified as AWS
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileStorage (BucketType (..), ResourceType (..))
import Domain.Types.Slug (Slug (..), mkSlug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.StagedUploadCleanup (deleteFile)
import Effects.StagedUploads (claimAndRelocateUpload)
import Log qualified
import Lucid qualified
import Servant qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue running action.
handler ::
  EphemeralUploads.Id ->
  Maybe Cookie ->
  FormData ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler ephemeralUploadId cookie form =
  handleRedirectErrors "Ephemeral upload edit" (dashboardEphemeralUploadsLinks.list Nothing) $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff and admins can edit ephemeral uploads." userMetadata
    action user userMetadata ephemeralUploadId form
    let listUrl = rootLink $ dashboardEphemeralUploadsLinks.list Nothing
        banner = BannerParams Success "Ephemeral Updated" "The ephemeral upload has been updated successfully."
        redirectUrl = buildRedirectUrl listUrl banner
    pure $ Servant.addHeader redirectUrl (redirectWithBanner listUrl banner)

--------------------------------------------------------------------------------

-- | Business logic: fetch, validate, update.
action ::
  User.Model ->
  UserMetadata.Model ->
  EphemeralUploads.Id ->
  FormData ->
  ExceptT HandlerError AppM ()
action user _userMetadata ephemeralUploadId form = do
  -- 1. Fetch ephemeral upload
  ephemeralUpload <- fetchEphemeralUpload ephemeralUploadId

  -- 2. Validate and sanitize title
  let title = Sanitize.sanitizePlainText (fdTitle form)
  when (Text.null title) $ throwValidationError "Title is required."

  -- 3. Validate and sanitize description
  let description = Sanitize.sanitizePlainText (fdDescription form)
  when (Text.length description < 80) $
    throwValidationError "Description must be at least 80 characters (approximately 2 sentences)."

  -- 4. Process audio replacement or keep existing
  let audioToken = fdAudioToken form
  (storagePath, mimeType, fileSize) <-
    if Text.null audioToken
      then pure (ephemeralUpload.eumAudioFilePath, ephemeralUpload.eumMimeType, ephemeralUpload.eumFileSize)
      else processNewAudio user ephemeralUpload title audioToken

  -- 5. Update the database record
  _ <-
    fromMaybeM (throwNotFound "Ephemeral upload") $
      fromRightM throwDatabaseError $
        execQuery $
          EphemeralUploads.updateEphemeralUpload
            ephemeralUploadId
            title
            description
            storagePath
            mimeType
            fileSize

  Log.logInfo "Ephemeral upload updated successfully" title

--------------------------------------------------------------------------------
-- Helpers

fetchEphemeralUpload ::
  EphemeralUploads.Id ->
  ExceptT HandlerError AppM EphemeralUploads.Model
fetchEphemeralUpload ephemeralUploadId =
  fromMaybeM (throwNotFound "Ephemeral upload") $
    fromRightM throwDatabaseError $
      execQuery (EphemeralUploads.getEphemeralUploadById ephemeralUploadId)

-- | Process a new audio upload: claim staged upload, delete old file, return new metadata.
processNewAudio ::
  User.Model ->
  EphemeralUploads.Model ->
  -- | Sanitized title
  Text ->
  -- | Audio token
  Text ->
  -- | (storagePath, mimeType, fileSize)
  ExceptT HandlerError AppM (Text, Text, Int64)
processNewAudio user ephemeralUpload title audioToken = do
  -- Get the staged upload to retrieve metadata
  stagedUpload <-
    fromMaybeM (throwValidationError "Uploaded file not found or expired.") $
      fromRightM throwDatabaseError $
        execQuery (StagedUploads.getByToken (StagedUploads.Token audioToken))

  -- Claim and relocate the staged upload to final location
  now <- liftIO getCurrentTime
  let Slug titleSlug = mkSlug title
  claimResult <-
    lift $
      claimAndRelocateUpload
        (User.mId user)
        audioToken
        StagedUploads.EphemeralAudio
        AudioBucket
        EphemeralAudio
        now
        titleSlug
  newStoragePath <- case claimResult of
    Left err -> do
      Log.logInfo "Failed to claim staged upload" err
      throwValidationError err
    Right path -> pure path

  -- Delete old audio file (best effort)
  backend <- asks (Has.getter @StorageBackend)
  mAwsEnv <- asks (Has.getter @(Maybe AWS.Env))
  fileDeleted <- deleteFile backend mAwsEnv ephemeralUpload.eumAudioFilePath
  unless fileDeleted $
    Log.logInfo "Failed to delete old ephemeral upload file" ephemeralUpload.eumAudioFilePath

  pure (newStoragePath, StagedUploads.mimeType stagedUpload, StagedUploads.fileSize stagedUpload)
