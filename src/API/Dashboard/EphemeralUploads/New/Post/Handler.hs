{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.EphemeralUploads.New.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.New.Post.Route (FormData (..))
import API.Links (apiLinks, dashboardEphemeralUploadsLinks)
import API.Types
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad.IO.Class (liftIO)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileStorage (BucketType (..), ResourceType (..))
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.StagedUploads (claimAndRelocateUpload)
import Log qualified
import Lucid qualified
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  FormData ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler cookie form =
  handleRedirectErrors "Ephemeral upload" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to upload ephemeral clips." userMetadata

    -- 2. Process the upload
    processEphemeralUpload user form

--------------------------------------------------------------------------------

-- | Process ephemeral upload and return appropriate response
processEphemeralUpload ::
  User.Model ->
  FormData ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
processEphemeralUpload user form = do
  let newUrl = Links.linkURI dashboardEphemeralUploadsLinks.newGet
      newUrlText = [i|/#{newUrl}|] :: Text
      listUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.list Nothing
      listUrlText = [i|/#{listUrl}|] :: Text

  -- Validate and sanitize title
  let title = Sanitize.sanitizePlainText (fdTitle form)
  if Text.null title
    then do
      let banner = BannerParams Error "Upload Failed" "Title is required."
      pure $ Servant.addHeader (buildRedirectUrl newUrlText banner) (redirectWithBanner newUrlText banner)
    else do
      -- Validate audio token is provided
      let audioToken = fdAudioToken form
      if Text.null audioToken
        then do
          let banner = BannerParams Error "Upload Failed" "Audio file is required."
          pure $ Servant.addHeader (buildRedirectUrl newUrlText banner) (redirectWithBanner newUrlText banner)
        else do
          -- First, get the staged upload to retrieve metadata
          stagedUploadResult <- execQuery (StagedUploads.getByToken (StagedUploads.Token audioToken))
          case stagedUploadResult of
            Left err -> do
              Log.logInfo "Failed to get staged upload" (Text.pack $ show err)
              throwDatabaseError err
            Right Nothing -> do
              Log.logInfo_ "Staged upload not found"
              let banner = BannerParams Error "Upload Failed" "Uploaded file not found or expired."
              pure $ Servant.addHeader (buildRedirectUrl newUrlText banner) (redirectWithBanner newUrlText banner)
            Right (Just stagedUpload) -> do
              -- Get current time for date hierarchy
              now <- liftIO getCurrentTime

              -- Claim and relocate the staged upload to final location
              claimResult <-
                claimAndRelocateUpload
                  (User.mId user)
                  audioToken
                  StagedUploads.EphemeralAudio
                  AudioBucket
                  EphemeralAudio
                  now
                  "ephemeral"

              case claimResult of
                Left err -> do
                  Log.logInfo "Failed to claim staged upload" err
                  let banner = BannerParams Error "Upload Failed" err
                  pure $ Servant.addHeader (buildRedirectUrl newUrlText banner) (redirectWithBanner newUrlText banner)
                Right storagePath -> do
                  -- Create the ephemeral upload record
                  let insert =
                        EphemeralUploads.Insert
                          { EphemeralUploads.euiTitle = title,
                            EphemeralUploads.euiAudioFilePath = storagePath,
                            EphemeralUploads.euiMimeType = StagedUploads.mimeType stagedUpload,
                            EphemeralUploads.euiFileSize = StagedUploads.fileSize stagedUpload,
                            EphemeralUploads.euiCreatorId = User.mId user
                          }

                  insertResult <- execQuery (EphemeralUploads.insertEphemeralUpload insert)
                  case insertResult of
                    Left err -> do
                      Log.logInfo "Failed to insert ephemeral upload" (Text.pack $ show err)
                      throwDatabaseError err
                    Right Nothing -> do
                      Log.logInfo_ "Ephemeral upload insert returned Nothing"
                      let banner = BannerParams Error "Upload Failed" "Failed to create ephemeral upload record."
                      pure $ Servant.addHeader (buildRedirectUrl newUrlText banner) (redirectWithBanner newUrlText banner)
                    Right (Just _ephemeralUploadId) -> do
                      Log.logInfo "Ephemeral upload completed successfully" title
                      let banner = BannerParams Success "Ephemeral Uploaded" "Your ephemeral clip has been uploaded successfully."
                          redirectUrl = buildRedirectUrl listUrlText banner
                      pure $ Servant.addHeader redirectUrl (redirectWithBanner listUrlText banner)
