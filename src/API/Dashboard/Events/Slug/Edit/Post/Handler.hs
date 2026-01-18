{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Edit.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Edit.Post.Route (EventEditForm (..), parseDateTime, parseStatus)
import API.Links (dashboardEventsLinks, rootLink)
import API.Types (DashboardEventsRoutes (..))
import App.Handler.Combinators (requireAuth, requireJust, requireRight, requireStaffNotSuspended)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (uploadEventPosterImage)
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  EventEditForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer eventId slug cookie editForm =
  handleRedirectErrors "Event update" (dashboardEventsLinks.editGet eventId slug) $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to edit events." userMetadata

    -- 2. Fetch event
    mResult <- execQuerySpan $ Events.getEventById eventId

    event <- case mResult of
      Left err -> throwDatabaseError err
      Right Nothing -> throwNotFound "Event"
      Right (Just e) -> pure e

    -- 3. Check authorization: must be staff/admin or the creator
    if not (event.emAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole)
      then throwNotAuthorized "You can only edit events you created or have staff permissions."
      else updateEvent eventId event editForm

updateEvent ::
  Events.Id ->
  Events.Model ->
  EventEditForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
updateEvent eventId event editForm = do
  -- 4. Parse and validate form data
  parsedStatus <- requireJust "Invalid event status value." (parseStatus (eefStatus editForm))
  parsedStartsAt <- requireJust "Invalid start date/time format." (parseDateTime (eefStartsAt editForm))
  parsedEndsAt <- requireJust "Invalid end date/time format." (parseDateTime (eefEndsAt editForm))

  -- 5. Sanitize and validate content
  let sanitizedTitle = Sanitize.sanitizeTitle (eefTitle editForm)
      sanitizedDescription = Sanitize.sanitizeUserContent (eefDescription editForm)
      sanitizedLocationName = Sanitize.sanitizePlainText (eefLocationName editForm)
      sanitizedLocationAddress = Sanitize.sanitizeDescription (eefLocationAddress editForm)

  validTitle <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 200 sanitizedTitle)
  validDescription <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 5000 sanitizedDescription)
  validLocationName <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 100 sanitizedLocationName)
  validLocationAddress <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 500 sanitizedLocationAddress)

  -- 6. Upload poster image if provided, or clear if requested
  posterImagePath <- case eefPosterImage editForm of
    Nothing ->
      -- No new file: check if user wants to clear existing image
      if eefPosterImageClear editForm
        then do
          Log.logInfo "Clearing poster image" event.emId
          pure Nothing
        else pure event.emPosterImageUrl -- Keep existing image
    Just posterImageFile -> do
      let newSlug = Slug.mkSlug validTitle
      storageBackend <- asks getter
      mAwsEnv <- asks getter
      uploadResult <- uploadEventPosterImage storageBackend mAwsEnv newSlug posterImageFile
      case uploadResult of
        Left uploadError -> do
          Log.logInfo "Poster image upload failed" (Aeson.object ["error" Aeson..= Text.pack (show uploadError)])
          pure event.emPosterImageUrl -- Keep existing image on error
        Right Nothing ->
          -- No file selected: check if user wants to clear existing image
          if eefPosterImageClear editForm
            then do
              Log.logInfo "Clearing poster image" event.emId
              pure Nothing
            else pure event.emPosterImageUrl -- Keep existing
        Right (Just result) -> do
          Log.logInfo "Poster image uploaded successfully" (Aeson.object ["path" Aeson..= uploadResultStoragePath result])
          pure (Just $ Text.pack $ uploadResultStoragePath result)

  -- 7. Build update data
  let newSlug = Slug.mkSlug validTitle
      updateData =
        Events.Insert
          { Events.eiTitle = validTitle,
            Events.eiSlug = newSlug,
            Events.eiDescription = validDescription,
            Events.eiStartsAt = parsedStartsAt,
            Events.eiEndsAt = parsedEndsAt,
            Events.eiLocationName = validLocationName,
            Events.eiLocationAddress = validLocationAddress,
            Events.eiStatus = parsedStatus,
            Events.eiAuthorId = event.emAuthorId,
            Events.eiPosterImageUrl = posterImagePath
          }

  -- 8. Update the event in a transaction
  mUpdateResult <- execTransactionSpan $ runMaybeT $ do
    _ <- MaybeT $ HT.statement () (Events.updateEvent event.emId updateData)
    pure ()

  case mUpdateResult of
    Left err -> throwDatabaseError err
    Right Nothing -> throwDatabaseError (error "Event update returned Nothing")
    Right (Just _) -> do
      Log.logInfo "Successfully updated event" event.emId
      let detailUrl = rootLink $ dashboardEventsLinks.detail eventId newSlug
          banner = BannerParams Success "Event Updated" "Your event has been updated and saved."
          redirectUrl = buildRedirectUrl detailUrl banner
      pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)
