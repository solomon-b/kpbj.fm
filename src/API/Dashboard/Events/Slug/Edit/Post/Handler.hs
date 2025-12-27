{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Events.Slug.Edit.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Edit.Post.Route (EventEditForm (..), parseDateTime, parseStatus)
import API.Links (apiLinks, dashboardEventsLinks, userLinks)
import API.Types (DashboardEventsRoutes (..), Routes (..), UserRoutes (..))
import App.Common (getUserInfo)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (stripStorageRoot, uploadEventPosterImage)
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

eventsListUrl :: Links.URI
eventsListUrl = Links.linkURI $ dashboardEventsLinks.list Nothing

eventsEditGetUrl :: Events.Id -> Slug -> Text
eventsEditGetUrl eid slug =
  let uri = Links.linkURI $ dashboardEventsLinks.editGet eid slug
   in [i|/#{uri}|]

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  EventEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer eventId slug cookie editForm = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized event edit attempt" eventId
      let banner = BannerParams Error "Login Required" "You must be logged in to edit events."
      pure $ Servant.noHeader (redirectWithBanner [i|/#{userLoginGetUrl}|] banner)
    Just (_user, userMetadata)
      | not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) || isSuspended userMetadata -> do
          let banner = BannerParams Error "Staff Access Required" "You do not have permission to edit events."
          pure $ Servant.noHeader (redirectWithBanner [i|/#{rootGetUrl}|] banner)
    Just (user, userMetadata) -> do
      mResult <- execTransactionSpan $ runMaybeT $ do
        event <- MaybeT $ HT.statement () (Events.getEventById eventId)
        MaybeT $ pure $ Just event

      case mResult of
        Left err -> do
          Log.logAttention "getEventById execution error" (show err)
          let banner = BannerParams Warning "Event Not Found" "The event you're trying to update doesn't exist."
          pure $ Servant.noHeader (redirectWithBanner [i|/#{eventsListUrl}|] banner)
        Right Nothing -> do
          Log.logInfo "No event found with id" eventId
          let banner = BannerParams Warning "Event Not Found" "The event you're trying to update doesn't exist."
          pure $ Servant.noHeader (redirectWithBanner [i|/#{eventsListUrl}|] banner)
        Right (Just event) ->
          if event.emAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole
            then updateEvent eventId slug event editForm
            else do
              Log.logInfo "User attempted to edit event they don't own" event.emId
              let banner = BannerParams Error "Access Denied" "You can only edit events you created or have staff permissions."
              pure $ Servant.noHeader (redirectWithBanner [i|/#{eventsListUrl}|] banner)

updateEvent ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Events.Id ->
  Slug ->
  Events.Model ->
  EventEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
updateEvent eventId slug event editForm = do
  let editUrl = eventsEditGetUrl eventId slug
  -- Parse and validate form data
  case (parseStatus (eefStatus editForm), parseDateTime (eefStartsAt editForm), parseDateTime (eefEndsAt editForm)) of
    (Nothing, _, _) -> do
      Log.logInfo "Invalid status in event edit form" (eefStatus editForm)
      let banner = BannerParams Error "Update Failed" "Invalid event status value."
      pure $ Servant.noHeader (redirectWithBanner editUrl banner)
    (_, Nothing, _) -> do
      Log.logInfo "Invalid starts_at datetime in event edit form" (eefStartsAt editForm)
      let banner = BannerParams Error "Update Failed" "Invalid start date/time format."
      pure $ Servant.noHeader (redirectWithBanner editUrl banner)
    (_, _, Nothing) -> do
      Log.logInfo "Invalid ends_at datetime in event edit form" (eefEndsAt editForm)
      let banner = BannerParams Error "Update Failed" "Invalid end date/time format."
      pure $ Servant.noHeader (redirectWithBanner editUrl banner)
    (Just parsedStatus, Just parsedStartsAt, Just parsedEndsAt) -> do
      -- Sanitize user input to prevent XSS attacks
      let sanitizedTitle = Sanitize.sanitizeTitle (eefTitle editForm)
          sanitizedDescription = Sanitize.sanitizeUserContent (eefDescription editForm)
          sanitizedLocationName = Sanitize.sanitizePlainText (eefLocationName editForm)
          sanitizedLocationAddress = Sanitize.sanitizeDescription (eefLocationAddress editForm)

      -- Validate content lengths
      case ( Sanitize.validateContentLength 200 sanitizedTitle,
             Sanitize.validateContentLength 5000 sanitizedDescription,
             Sanitize.validateContentLength 100 sanitizedLocationName,
             Sanitize.validateContentLength 500 sanitizedLocationAddress
           ) of
        (Left titleError, _, _, _) -> do
          let errorMsg = Sanitize.displayContentValidationError titleError
              banner = BannerParams Error "Update Failed" errorMsg
          pure $ Servant.noHeader (redirectWithBanner editUrl banner)
        (_, Left descError, _, _) -> do
          let errorMsg = Sanitize.displayContentValidationError descError
              banner = BannerParams Error "Update Failed" errorMsg
          pure $ Servant.noHeader (redirectWithBanner editUrl banner)
        (_, _, Left nameError, _) -> do
          let errorMsg = Sanitize.displayContentValidationError nameError
              banner = BannerParams Error "Update Failed" errorMsg
          pure $ Servant.noHeader (redirectWithBanner editUrl banner)
        (_, _, _, Left addrError) -> do
          let errorMsg = Sanitize.displayContentValidationError addrError
              banner = BannerParams Error "Update Failed" errorMsg
          pure $ Servant.noHeader (redirectWithBanner editUrl banner)
        (Right validTitle, Right validDescription, Right validLocationName, Right validLocationAddress) -> do
          -- Upload poster image if provided
          posterImagePath <- case eefPosterImage editForm of
            Nothing -> pure event.emPosterImageUrl -- Keep existing image
            Just posterImageFile -> do
              let newSlug = Slug.mkSlug validTitle
              uploadResult <- uploadEventPosterImage newSlug posterImageFile
              case uploadResult of
                Left uploadError -> do
                  Log.logInfo "Poster image upload failed" (Aeson.object ["error" Aeson..= Text.pack (show uploadError)])
                  pure event.emPosterImageUrl -- Keep existing image on error
                Right Nothing -> pure event.emPosterImageUrl -- No file selected, keep existing
                Right (Just result) -> do
                  Log.logInfo "Poster image uploaded successfully" (Aeson.object ["path" Aeson..= uploadResultStoragePath result])
                  pure (Just $ stripStorageRoot $ uploadResultStoragePath result)

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

          -- Update the event in a transaction
          mUpdateResult <- execTransactionSpan $ runMaybeT $ do
            -- Update event
            _ <- MaybeT $ HT.statement () (Events.updateEvent event.emId updateData)
            MaybeT $ pure $ Just ()

          case mUpdateResult of
            Left err -> do
              Log.logInfo "Failed to update event" (event.emId, show err)
              let banner = BannerParams Error "Update Failed" "Database error occurred. Please try again."
              pure $ Servant.noHeader (redirectWithBanner editUrl banner)
            Right Nothing -> do
              Log.logInfo "Event update returned Nothing" event.emId
              let banner = BannerParams Error "Update Failed" "Failed to update event. Please try again."
              pure $ Servant.noHeader (redirectWithBanner editUrl banner)
            Right (Just _) -> do
              Log.logInfo "Successfully updated event" event.emId
              let detailLink = Links.linkURI $ dashboardEventsLinks.detail eventId newSlug
                  detailUrl = [i|/#{detailLink}|] :: Text
                  banner = BannerParams Success "Event Updated" "Your event has been updated and saved."
                  redirectUrl = buildRedirectUrl detailUrl banner
              pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)
