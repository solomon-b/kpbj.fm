module API.Dashboard.Events.New.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.New.Post.Route (NewEventForm (..))
import API.Links (dashboardEventsLinks, rootLink)
import API.Types (DashboardEventsRoutes (..))
import Amazonka qualified as AWS
import App.Handler.Combinators (requireAuth, requireRight, requireStaffNotSuspended)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has, getter)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug ()
import Domain.Types.Slug qualified as Slug
import Domain.Types.StorageBackend (StorageBackend)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.FileUpload (uploadEventPosterImage)
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified

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
  | otherwise = case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" (Text.unpack dateTimeStr) of
      Nothing -> Left [fieldName <> " is not in a valid format"]
      Just utcTime -> Right utcTime

validateStatus :: Text -> Either [Text] Events.Status
validateStatus status = case status of
  "draft" -> Right Events.Draft
  "published" -> Right Events.Published
  _ -> Left ["Invalid status selected"]

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadMask m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env,
    Has StorageBackend env,
    Has (Maybe AWS.Env) env
  ) =>
  Tracer ->
  Maybe Cookie ->
  NewEventForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer cookie form =
  handleRedirectErrors "Event creation" dashboardEventsLinks.newGet $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to create events." userMetadata

    -- 2. Upload poster image if provided
    posterImagePath <- handlePosterUpload form

    -- 3. Validate form
    (title, description, startsAt, endsAt, locationName, locationAddress, status) <-
      requireRight Sanitize.displayContentValidationError (validateEventForm form)

    -- 4. Insert event
    let slug = Slug.mkSlug title
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
              Events.eiPosterImageUrl = posterImagePath
            }
    eventId <-
      execQuerySpan (Events.insertEvent eventInsert) >>= \case
        Left err -> throwDatabaseError err
        Right eid -> pure eid

    -- 5. Fetch created event and redirect
    Log.logInfo "Event created successfully" eventId
    execQuerySpan (Events.getEventById eventId) >>= \case
      Right (Just event) -> do
        let eventSlug = Events.emSlug event
            detailUrl = rootLink $ dashboardEventsLinks.detail eventId eventSlug
            banner = BannerParams Success "Event Created" "Your event has been created successfully."
            redirectUrl = buildRedirectUrl detailUrl banner
        pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)
      _ -> do
        Log.logInfo "Failed to fetch event" (Aeson.object ["eventId" .= eventId])
        let listUrl = rootLink $ dashboardEventsLinks.list Nothing
            banner = BannerParams Success "Event Created" "Your event has been created."
            redirectUrl = buildRedirectUrl listUrl banner
        pure $ Servant.addHeader redirectUrl (redirectWithBanner listUrl banner)

-- | Handle poster image upload
handlePosterUpload ::
  (MonadUnliftIO m, Log.MonadLog m, MonadMask m, MonadIO m, MonadReader env m, Has StorageBackend env, Has (Maybe AWS.Env) env) =>
  NewEventForm ->
  m (Maybe Text)
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
