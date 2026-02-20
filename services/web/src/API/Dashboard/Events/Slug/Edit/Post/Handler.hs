{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Edit.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Edit.Post.Route (EventEditForm (..), parseDateTime, parseStatus)
import API.Links (dashboardEventsLinks, rootLink)
import API.Types (DashboardEventsRoutes (..))
import App.Handler.Combinators (requireAuth, requireJust, requireRight, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (unless, void, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
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
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (uploadEventPosterImage)
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Servant qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to build the post-update redirect.
data EventEditRedirectData = EventEditRedirectData
  { eerRedirectUrl :: Text,
    eerBanner :: BannerParams
  }

-- | Business logic: fetch event, authorize, validate, update, build redirect data.
action ::
  User.Model ->
  UserMetadata.Model ->
  Events.Id ->
  Slug ->
  EventEditForm ->
  ExceptT HandlerError AppM EventEditRedirectData
action user userMetadata eventId _slug editForm = do
  -- 1. Fetch event
  event <-
    fromMaybeM (throwNotFound "Event") $
      fromRightM throwDatabaseError $
        execQuery (Events.getEventById eventId)

  -- 2. Check authorization: must be staff/admin or the creator
  unless (event.emAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole) $
    throwNotAuthorized "You can only edit events you created or have staff permissions." (Just userMetadata.mUserRole)

  updateEvent eventId event editForm

-- | Servant handler: thin glue composing action + building redirect response.
handler ::
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  EventEditForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler eventId slug cookie editForm =
  handleRedirectErrors "Event update" (dashboardEventsLinks.editGet eventId slug) $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to edit events." userMetadata
    vd <- action user userMetadata eventId slug editForm
    pure $ Servant.addHeader vd.eerRedirectUrl (redirectWithBanner vd.eerRedirectUrl vd.eerBanner)

updateEvent ::
  Events.Id ->
  Events.Model ->
  EventEditForm ->
  ExceptT HandlerError AppM EventEditRedirectData
updateEvent eventId event editForm = do
  -- 3. Parse and validate form data
  parsedStatus <- requireJust "Invalid event status value." (parseStatus (eefStatus editForm))
  parsedStartsAt <- requireJust "Invalid start date/time format." (parseDateTime (eefStartsAt editForm))
  parsedEndsAt <- requireJust "Invalid end date/time format." (parseDateTime (eefEndsAt editForm))

  -- 4. Sanitize and validate content
  let sanitizedTitle = Sanitize.sanitizeTitle (eefTitle editForm)
      sanitizedDescription = Sanitize.sanitizeUserContent (eefDescription editForm)
      sanitizedLocationName = Sanitize.sanitizePlainText (eefLocationName editForm)
      sanitizedLocationAddress = Sanitize.sanitizeDescription (eefLocationAddress editForm)

  validTitle <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 200 sanitizedTitle)
  validDescription <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 5000 sanitizedDescription)
  validLocationName <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 100 sanitizedLocationName)
  validLocationAddress <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 500 sanitizedLocationAddress)

  -- 5. Upload poster image if provided, or clear if requested
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
      uploadResult <- lift $ uploadEventPosterImage storageBackend mAwsEnv newSlug posterImageFile
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

  -- 6. Build update data
  let featuredOnHomepage = eefFeaturedOnHomepage editForm == "true"
      newSlug = Slug.mkSlug validTitle
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
            Events.eiPosterImageUrl = posterImagePath,
            Events.eiFeaturedOnHomepage = featuredOnHomepage
          }

  -- 7. Update the event in a transaction
  mUpdateResult <-
    fromRightM throwDatabaseError $
      execTransaction $
        runMaybeT $ do
          when featuredOnHomepage $
            lift $
              void $
                HT.statement () Events.clearFeaturedEvents
          _ <- MaybeT $ HT.statement () (Events.updateEvent event.emId updateData)
          pure ()

  case mUpdateResult of
    Nothing -> throwHandlerFailure "Event update returned Nothing"
    Just _ -> do
      Log.logInfo "Successfully updated event" event.emId
      let detailUrl = rootLink $ dashboardEventsLinks.detail eventId newSlug
          banner = BannerParams Success "Event Updated" "Your event has been updated and saved."
          redirectUrl = buildRedirectUrl detailUrl banner
      pure $ EventEditRedirectData redirectUrl banner
