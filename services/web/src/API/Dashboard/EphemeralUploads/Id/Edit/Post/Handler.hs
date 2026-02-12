{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.EphemeralUploads.Id.Edit.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Id.Edit.Post.Route (FormData (..))
import API.Links (dashboardEphemeralUploadsLinks)
import API.Types
import Amazonka qualified as AWS
import App.Handler.Combinators (requireAuth)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.String.Interpolate (i)
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
import Hasql.Pool qualified
import Log qualified
import Lucid qualified
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

handler ::
  EphemeralUploads.Id ->
  Maybe Cookie ->
  FormData ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler ephemeralUploadId cookie form =
  handleRedirectErrors "Ephemeral upload edit" (dashboardEphemeralUploadsLinks.list Nothing) $ do
    -- 1. Require authentication and staff/admin role
    (user, userMetadata) <- requireAuth cookie

    -- 2. Check authorization: must be staff/admin
    let isStaffOrAdmin = UserMetadata.isStaffOrHigher userMetadata.mUserRole
    unless isStaffOrAdmin $
      throwNotAuthorized "Only staff and admins can edit ephemeral uploads." (Just userMetadata.mUserRole)

    -- 3. Fetch ephemeral upload
    ephemeralUpload <- fetchEphemeralUpload ephemeralUploadId

    -- 4. Process the update
    processEphemeralUploadEdit user ephemeralUpload form

--------------------------------------------------------------------------------
-- Helpers

fetchEphemeralUpload ::
  EphemeralUploads.Id ->
  AppM EphemeralUploads.Model
fetchEphemeralUpload ephemeralUploadId =
  execQuery (EphemeralUploads.getEphemeralUploadById ephemeralUploadId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Ephemeral upload"
    Right (Just ephemeralUpload) -> pure ephemeralUpload

processEphemeralUploadEdit ::
  User.Model ->
  EphemeralUploads.Model ->
  FormData ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
processEphemeralUploadEdit user ephemeralUpload form = do
  let ephemeralUploadId = ephemeralUpload.eumId
      editUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.editGet ephemeralUploadId
      editUrlText = [i|/#{editUrl}|] :: Text
      listUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.list Nothing
      listUrlText = [i|/#{listUrl}|] :: Text

  -- Validate and sanitize title
  let title = Sanitize.sanitizePlainText (fdTitle form)
  if Text.null title
    then do
      let banner = BannerParams Error "Update Failed" "Title is required."
      pure $ Servant.addHeader (buildRedirectUrl editUrlText banner) (redirectWithBanner editUrlText banner)
    else do
      -- Validate and sanitize description
      let description = Sanitize.sanitizePlainText (fdDescription form)
      if Text.length description < 80
        then do
          let banner = BannerParams Error "Update Failed" "Description must be at least 80 characters (approximately 2 sentences)."
          pure $ Servant.addHeader (buildRedirectUrl editUrlText banner) (redirectWithBanner editUrlText banner)
        else do
          -- Check if we have a new audio file to upload
          let audioToken = fdAudioToken form
          if Text.null audioToken
            then do
              -- No new audio, just update title and description (keep existing audio fields)
              updateResult <-
                execQuery $
                  EphemeralUploads.updateEphemeralUpload
                    ephemeralUploadId
                    title
                    description
                    ephemeralUpload.eumAudioFilePath
                    ephemeralUpload.eumMimeType
                    ephemeralUpload.eumFileSize
              handleUpdateResult title listUrlText updateResult
            else do
              -- New audio file uploaded - process it
              processWithNewAudio user ephemeralUpload title description audioToken editUrlText listUrlText

processWithNewAudio ::
  User.Model ->
  EphemeralUploads.Model ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
processWithNewAudio user ephemeralUpload title description audioToken editUrlText listUrlText = do
  let ephemeralUploadId = ephemeralUpload.eumId

  -- Get the staged upload to retrieve metadata
  stagedUploadResult <- execQuery (StagedUploads.getByToken (StagedUploads.Token audioToken))
  case stagedUploadResult of
    Left err -> do
      Log.logInfo "Failed to get staged upload" (Text.pack $ show err)
      throwDatabaseError err
    Right Nothing -> do
      Log.logInfo_ "Staged upload not found"
      let banner = BannerParams Error "Update Failed" "Uploaded file not found or expired."
      pure $ Servant.addHeader (buildRedirectUrl editUrlText banner) (redirectWithBanner editUrlText banner)
    Right (Just stagedUpload) -> do
      -- Get current time for date hierarchy
      now <- liftIO getCurrentTime

      -- Claim and relocate the staged upload to final location
      let Slug titleSlug = mkSlug title
      claimResult <-
        claimAndRelocateUpload
          (User.mId user)
          audioToken
          StagedUploads.EphemeralAudio
          AudioBucket
          EphemeralAudio
          now
          titleSlug

      case claimResult of
        Left err -> do
          Log.logInfo "Failed to claim staged upload" err
          let banner = BannerParams Error "Update Failed" err
          pure $ Servant.addHeader (buildRedirectUrl editUrlText banner) (redirectWithBanner editUrlText banner)
        Right newStoragePath -> do
          -- Delete old audio file (best effort)
          backend <- asks (Has.getter @StorageBackend)
          mAwsEnv <- asks (Has.getter @(Maybe AWS.Env))
          fileDeleted <- deleteFile backend mAwsEnv ephemeralUpload.eumAudioFilePath
          unless fileDeleted $
            Log.logInfo "Failed to delete old ephemeral upload file" ephemeralUpload.eumAudioFilePath

          -- Update the database record with new audio info
          updateResult <-
            execQuery $
              EphemeralUploads.updateEphemeralUpload
                ephemeralUploadId
                title
                description
                newStoragePath
                (StagedUploads.mimeType stagedUpload)
                (StagedUploads.fileSize stagedUpload)
          handleUpdateResult title listUrlText updateResult

handleUpdateResult ::
  Text ->
  Text ->
  Either Hasql.Pool.UsageError (Maybe EphemeralUploads.Model) ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handleUpdateResult title listUrlText = \case
  Left err -> do
    Log.logInfo "Failed to update ephemeral upload" (Text.pack $ show err)
    throwDatabaseError err
  Right Nothing -> do
    Log.logInfo_ "Ephemeral upload not found during update"
    throwNotFound "Ephemeral upload"
  Right (Just _) -> do
    Log.logInfo "Ephemeral upload updated successfully" title
    let banner = BannerParams Success "Ephemeral Updated" "The ephemeral upload has been updated successfully."
        redirectUrl = buildRedirectUrl listUrlText banner
    pure $ Servant.addHeader redirectUrl (redirectWithBanner listUrlText banner)
