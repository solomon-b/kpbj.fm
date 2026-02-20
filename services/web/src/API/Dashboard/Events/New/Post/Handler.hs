module API.Dashboard.Events.New.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.New.Post.Route (NewEventForm (..))
import API.Links (dashboardEventsLinks, rootLink)
import API.Types (DashboardEventsRoutes (..))
import App.Handler.Combinators (requireAuth, requireRight, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug ()
import Domain.Types.Slug qualified as Slug
import Domain.Types.Timezone (parsePacificFromDateTimeInput)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.FileUpload (uploadEventPosterImage)
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Servant qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- Validation functions
validateEventForm :: NewEventForm -> Either Sanitize.ContentValidationError (Text, Text, UTCTime, UTCTime, Text, Text, Events.Status)
validateEventForm form = do
  -- Sanitize and validate inputs
  let sanitizedTitle = Sanitize.sanitizeTitle form.nefTitle
      sanitizedDescription = Sanitize.sanitizeUserContent form.nefDescription
      sanitizedLocationName = Sanitize.sanitizePlainText form.nefLocationName
      sanitizedLocationAddress = Sanitize.sanitizeDescription form.nefLocationAddress

  -- Validate content lengths and requirements
  validTitle <- Sanitize.validateContentLength 200 sanitizedTitle
  validDescription <- Sanitize.validateContentLength 5000 sanitizedDescription
  validLocationName <- Sanitize.validateContentLength 100 sanitizedLocationName
  validLocationAddress <- Sanitize.validateContentLength 500 sanitizedLocationAddress

  -- Parse dates and status using existing validation functions
  startsAt <- case validateDateTime "Start date/time" form.nefStartsAt of
    Left errors -> Left $ Sanitize.ContentInvalid $ Text.intercalate ", " errors
    Right dt -> Right dt
  endsAt <- case validateDateTime "End date/time" form.nefEndsAt of
    Left errors -> Left $ Sanitize.ContentInvalid $ Text.intercalate ", " errors
    Right dt -> Right dt
  status <- case validateStatus form.nefStatus of
    Left errors -> Left $ Sanitize.ContentInvalid $ Text.intercalate ", " errors
    Right s -> Right s

  -- Validate that end time is after start time
  if endsAt <= startsAt
    then Left $ Sanitize.ContentInvalid "End time must be after start time"
    else Right (validTitle, validDescription, startsAt, endsAt, validLocationName, validLocationAddress, status)

validateDateTime :: Text -> Text -> Either [Text] UTCTime
validateDateTime fieldName dateTimeStr
  | Text.null (Text.strip dateTimeStr) = Left [fieldName <> " cannot be empty"]
  | otherwise = case parsePacificFromDateTimeInput dateTimeStr of
      Nothing -> Left [fieldName <> " is not in a valid format"]
      Just utcTime -> Right utcTime

validateStatus :: Text -> Either [Text] Events.Status
validateStatus status = case status of
  "draft" -> Right Events.Draft
  "published" -> Right Events.Published
  _ -> Left ["Invalid status selected"]

--------------------------------------------------------------------------------

-- | All data needed to build the post-creation redirect.
data NewEventRedirectData = NewEventRedirectData
  { nerRedirectUrl :: Text,
    nerBanner :: BannerParams
  }

-- | Business logic: validate, create event, build redirect data.
action ::
  User.Model ->
  NewEventForm ->
  ExceptT HandlerError AppM NewEventRedirectData
action user form = do
  -- 1. Upload poster image if provided
  posterImagePath <- lift $ handlePosterUpload form

  -- 2. Validate form
  (title, description, startsAt, endsAt, locationName, locationAddress, status) <-
    requireRight Sanitize.displayContentValidationError (validateEventForm form)

  -- 3. Insert event
  let featuredOnHomepage = nefFeaturedOnHomepage form == "true"
      slug = Slug.mkSlug title
      eventInsert =
        Events.Insert
          { Events.eiTitle = title,
            Events.eiSlug = slug,
            Events.eiDescription = description,
            Events.eiStartsAt = startsAt,
            Events.eiEndsAt = endsAt,
            Events.eiLocationName = locationName,
            Events.eiLocationAddress = locationAddress,
            Events.eiStatus = status,
            Events.eiAuthorId = User.mId user,
            Events.eiPosterImageUrl = posterImagePath,
            Events.eiFeaturedOnHomepage = featuredOnHomepage
          }
  eventId <-
    if featuredOnHomepage
      then do
        result <-
          fromRightM throwDatabaseError $
            execTransaction $ do
              void $ HT.statement () Events.clearFeaturedEvents
              HT.statement () (Events.insertEvent eventInsert)
        case result of
          Just eid -> pure eid
          Nothing -> throwHandlerFailure "Event insert returned Nothing"
      else do
        result <-
          fromRightM throwDatabaseError $
            execQuery (Events.insertEvent eventInsert)
        case result of
          Just eid -> pure eid
          Nothing -> throwHandlerFailure "Event insert returned Nothing"

  -- 4. Fetch created event and build redirect data
  Log.logInfo "Event created successfully" eventId
  fetchResult <- execQuery (Events.getEventById eventId)
  case fetchResult of
    Right (Just event) -> do
      let eventSlug = Events.emSlug event
          detailUrl = rootLink $ dashboardEventsLinks.detail eventId eventSlug
          banner = BannerParams Success "Event Created" "Your event has been created successfully."
          redirectUrl = buildRedirectUrl detailUrl banner
      pure $ NewEventRedirectData redirectUrl banner
    _ -> do
      Log.logInfo "Failed to fetch event" (Aeson.object ["eventId" .= eventId])
      let listUrl = rootLink $ dashboardEventsLinks.list Nothing
          banner = BannerParams Success "Event Created" "Your event has been created."
          redirectUrl = buildRedirectUrl listUrl banner
      pure $ NewEventRedirectData redirectUrl banner

-- | Servant handler: thin glue composing action + building redirect response.
handler ::
  Maybe Cookie ->
  NewEventForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler cookie form =
  handleRedirectErrors "Event creation" dashboardEventsLinks.newGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to create events." userMetadata
    vd <- action user form
    pure $ Servant.addHeader vd.nerRedirectUrl (redirectWithBanner vd.nerRedirectUrl vd.nerBanner)

-- | Handle poster image upload
handlePosterUpload ::
  NewEventForm ->
  AppM (Maybe Text)
handlePosterUpload form = case nefPosterImage form of
  Nothing -> pure Nothing
  Just posterImageFile -> do
    let slug = Slug.mkSlug (nefTitle form)
    storageBackend <- asks getter
    mAwsEnv <- asks getter
    uploadResult <- uploadEventPosterImage storageBackend mAwsEnv slug posterImageFile
    case uploadResult of
      Left uploadError -> do
        Log.logInfo "Poster image upload failed" (Aeson.object ["error" Aeson..= Text.pack (show uploadError)])
        pure Nothing
      Right Nothing -> pure Nothing
      Right (Just result) -> do
        Log.logInfo "Poster image uploaded successfully" (Aeson.object ["path" Aeson..= uploadResultStoragePath result])
        pure (Just $ Text.pack $ uploadResultStoragePath result)
