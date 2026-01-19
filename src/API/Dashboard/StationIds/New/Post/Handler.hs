{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StationIds.New.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StationIds.New.Post.Route (FormData (..))
import API.Links (apiLinks, dashboardStationIdsLinks)
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
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.StagedUploads (claimAndRelocateUpload)
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Maybe Cookie ->
  FormData ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer cookie form =
  handleRedirectErrors "Station ID upload" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to upload station IDs." userMetadata

    -- 2. Process the upload
    processStationIdUpload user form

--------------------------------------------------------------------------------

-- | Process station ID upload and return appropriate response
processStationIdUpload ::
  User.Model ->
  FormData ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
processStationIdUpload user form = do
  let newUrl = Links.linkURI dashboardStationIdsLinks.newGet
      newUrlText = [i|/#{newUrl}|] :: Text
      listUrl = Links.linkURI $ dashboardStationIdsLinks.list Nothing
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
          stagedUploadResult <- execQuerySpan (StagedUploads.getByToken (StagedUploads.Token audioToken))
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
                  StagedUploads.StationIdAudio
                  AudioBucket
                  StationIdAudio
                  now
                  "station-id"

              case claimResult of
                Left err -> do
                  Log.logInfo "Failed to claim staged upload" err
                  let banner = BannerParams Error "Upload Failed" err
                  pure $ Servant.addHeader (buildRedirectUrl newUrlText banner) (redirectWithBanner newUrlText banner)
                Right storagePath -> do
                  -- Create the station ID record
                  let insert =
                        StationIds.Insert
                          { StationIds.siiTitle = title,
                            StationIds.siiAudioFilePath = storagePath,
                            StationIds.siiMimeType = StagedUploads.mimeType stagedUpload,
                            StationIds.siiFileSize = StagedUploads.fileSize stagedUpload,
                            StationIds.siiCreatorId = User.mId user
                          }

                  insertResult <- execQuerySpan (StationIds.insertStationId insert)
                  case insertResult of
                    Left err -> do
                      Log.logInfo "Failed to insert station ID" (Text.pack $ show err)
                      throwDatabaseError err
                    Right Nothing -> do
                      Log.logInfo_ "Station ID insert returned Nothing"
                      let banner = BannerParams Error "Upload Failed" "Failed to create station ID record."
                      pure $ Servant.addHeader (buildRedirectUrl newUrlText banner) (redirectWithBanner newUrlText banner)
                    Right (Just _stationIdId) -> do
                      Log.logInfo "Station ID uploaded successfully" title
                      let banner = BannerParams Success "Station ID Uploaded" "Your station ID has been uploaded successfully."
                          redirectUrl = buildRedirectUrl listUrlText banner
                      pure $ Servant.addHeader redirectUrl (redirectWithBanner listUrlText banner)
