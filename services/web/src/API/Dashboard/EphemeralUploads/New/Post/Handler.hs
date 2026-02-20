module API.Dashboard.EphemeralUploads.New.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.New.Post.Route (FormData (..))
import API.Links (dashboardEphemeralUploadsLinks, rootLink)
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
import Domain.Types.Slug (Slug (..), mkSlug)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.StagedUploads (claimAndRelocateUpload)
import Log qualified
import Lucid qualified
import Servant qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue running action.
handler ::
  Maybe Cookie ->
  FormData ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler cookie form =
  handleRedirectErrors "Ephemeral upload" dashboardEphemeralUploadsLinks.newGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to upload ephemeral clips." userMetadata
    action user form
    let listUrl = rootLink $ dashboardEphemeralUploadsLinks.list Nothing
        banner = BannerParams Success "Ephemeral Uploaded" "Your ephemeral clip has been uploaded successfully."
        redirectUrl = buildRedirectUrl listUrl banner
    pure $ Servant.addHeader redirectUrl (redirectWithBanner listUrl banner)

--------------------------------------------------------------------------------

-- | Business logic: validate, upload, insert record.
action ::
  User.Model ->
  FormData ->
  ExceptT HandlerError AppM ()
action user form = do
  -- 1. Validate and sanitize title
  let title = Sanitize.sanitizePlainText (fdTitle form)
  when (Text.null title) $ throwValidationError "Title is required."

  -- 2. Validate and sanitize description
  let description = Sanitize.sanitizePlainText (fdDescription form)
  when (Text.length description < 80) $
    throwValidationError "Description must be at least 80 characters (approximately 2 sentences)."

  -- 3. Validate audio token is provided
  let audioToken = fdAudioToken form
  when (Text.null audioToken) $ throwValidationError "Audio file is required."

  -- 4. Get the staged upload to retrieve metadata
  stagedUpload <-
    fromMaybeM (throwValidationError "Uploaded file not found or expired.") $
      fromRightM throwDatabaseError $
        execQuery (StagedUploads.getByToken (StagedUploads.Token audioToken))

  -- 5. Claim and relocate the staged upload to final location
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
  storagePath <- case claimResult of
    Left err -> do
      Log.logInfo "Failed to claim staged upload" err
      throwValidationError err
    Right path -> pure path

  -- 6. Create the ephemeral upload record
  let insert =
        EphemeralUploads.Insert
          { EphemeralUploads.euiTitle = title,
            EphemeralUploads.euiDescription = description,
            EphemeralUploads.euiAudioFilePath = storagePath,
            EphemeralUploads.euiMimeType = StagedUploads.mimeType stagedUpload,
            EphemeralUploads.euiFileSize = StagedUploads.fileSize stagedUpload,
            EphemeralUploads.euiCreatorId = User.mId user
          }
  _ <-
    fromMaybeM (throwHandlerFailure "Failed to create ephemeral upload record.") $
      fromRightM throwDatabaseError $
        execQuery (EphemeralUploads.insertEphemeralUpload insert)

  Log.logInfo "Ephemeral upload completed successfully" title
