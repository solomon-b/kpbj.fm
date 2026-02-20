module API.Dashboard.StationIds.New.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.StationIds.New.Post.Route (FormData (..))
import API.Links (dashboardStationIdsLinks, rootLink)
import API.Types
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileStorage (BucketType (..), ResourceType (..))
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.StagedUploads (claimAndRelocateUpload)
import Log qualified
import Lucid qualified
import Servant qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + redirect.
handler ::
  Maybe Cookie ->
  FormData ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler cookie form =
  handleRedirectErrors "Station ID upload" dashboardStationIdsLinks.newGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to upload station IDs." userMetadata
    action user form
    let listUrl = rootLink $ dashboardStationIdsLinks.list Nothing
        banner = BannerParams Success "Station ID Uploaded" "Your station ID has been uploaded successfully."
        redirectUrl = buildRedirectUrl listUrl banner
    pure $ Servant.addHeader redirectUrl (redirectWithBanner listUrl banner)

--------------------------------------------------------------------------------

-- | Business logic: validate, upload, insert station ID record.
action ::
  User.Model ->
  FormData ->
  ExceptT HandlerError AppM ()
action user form = do
  -- 1. Validate and sanitize title
  let title = Sanitize.sanitizePlainText (fdTitle form)
  when (Text.null title) $ throwValidationError "Title is required."

  -- 2. Validate audio token is provided
  let audioToken = fdAudioToken form
  when (Text.null audioToken) $ throwValidationError "Audio file is required."

  -- 3. Get the staged upload to retrieve metadata
  stagedUpload <-
    fromMaybeM (throwValidationError "Uploaded file not found or expired.") $
      fromRightM throwDatabaseError $
        execQuery (StagedUploads.getByToken (StagedUploads.Token audioToken))

  -- 4. Claim and relocate the staged upload to final location
  now <- liftIO getCurrentTime
  claimResult <-
    lift $
      claimAndRelocateUpload
        (User.mId user)
        audioToken
        StagedUploads.StationIdAudio
        AudioBucket
        StationIdAudio
        now
        "station-id"
  storagePath <- case claimResult of
    Left err -> do
      Log.logInfo "Failed to claim staged upload" err
      throwValidationError err
    Right path -> pure path

  -- 5. Create the station ID record
  let insert =
        StationIds.Insert
          { StationIds.siiTitle = title,
            StationIds.siiAudioFilePath = storagePath,
            StationIds.siiMimeType = StagedUploads.mimeType stagedUpload,
            StationIds.siiFileSize = StagedUploads.fileSize stagedUpload,
            StationIds.siiCreatorId = User.mId user
          }
  _ <-
    fromMaybeM (throwHandlerFailure "Failed to create station ID record.") $
      fromRightM throwDatabaseError $
        execQuery (StationIds.insertStationId insert)

  Log.logInfo "Station ID uploaded successfully" title
