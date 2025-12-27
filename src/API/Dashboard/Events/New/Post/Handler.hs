{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Events.New.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.New.Post.Route (NewEventForm (..))
import API.Links (apiLinks, dashboardEventsLinks, userLinks)
import API.Types (DashboardEventsRoutes (..), Routes (..), UserRoutes (..))
import App.Common (getUserInfo)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug ()
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (stripStorageRoot, uploadEventPosterImage)
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardEventsNewGetUrl :: Links.URI
dashboardEventsNewGetUrl = Links.linkURI dashboardEventsLinks.newGet

dashboardEventsListUrl :: Links.URI
dashboardEventsListUrl = Links.linkURI $ dashboardEventsLinks.list Nothing

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

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
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Maybe Cookie ->
  NewEventForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer cookie form = do
  getUserInfo cookie >>= \case
    Nothing -> do
      let banner = BannerParams Error "Login Required" "You must be logged in to create events."
      pure (Servant.noHeader (redirectWithBanner [i|/#{userLoginGetUrl}|] banner))
    Just (_user, userMetadata)
      | not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) || isSuspended userMetadata -> do
          let banner = BannerParams Error "Staff Access Required" "You do not have permission to create events."
          pure (Servant.noHeader (redirectWithBanner [i|/#{rootGetUrl}|] banner))
    Just (user, _userMetadata) -> do
      -- Upload poster image if provided
      posterImagePath <- case nefPosterImage form of
        Nothing -> pure Nothing
        Just posterImageFile -> do
          let slug = Slug.mkSlug (nefTitle form)
          uploadResult <- uploadEventPosterImage slug posterImageFile
          case uploadResult of
            Left uploadError -> do
              Log.logInfo "Poster image upload failed" (Aeson.object ["error" Aeson..= Text.pack (show uploadError)])
              pure Nothing
            Right Nothing -> pure Nothing -- No file selected
            Right (Just result) -> do
              Log.logInfo "Poster image uploaded successfully" (Aeson.object ["path" Aeson..= uploadResultStoragePath result])
              pure (Just $ stripStorageRoot $ uploadResultStoragePath result)

      case validateEventForm form of
        Left validationError -> do
          let errorMsg = Sanitize.displayContentValidationError validationError
          Log.logInfo "Event creation failed validation" errorMsg
          let banner = BannerParams Error "Validation Error" errorMsg
          pure $ Servant.noHeader (redirectWithBanner [i|/#{dashboardEventsNewGetUrl}|] banner)
        Right (title, description, startsAt, endsAt, locationName, locationAddress, status) -> do
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
                    Events.eiAuthorId = user.mId,
                    Events.eiPosterImageUrl = posterImagePath
                  }
          execQuerySpan (Events.insertEvent eventInsert) >>= \case
            Left _err -> do
              Log.logInfo "Failed to create event in database" ()
              let banner = BannerParams Error "Database Error" "Database error occurred. Please try again."
              pure $ Servant.noHeader (redirectWithBanner [i|/#{dashboardEventsNewGetUrl}|] banner)
            Right eventId -> do
              Log.logInfo "Event created successfully" eventId
              execQuerySpan (Events.getEventById eventId) >>= \case
                Right (Just event) -> do
                  let eventSlug = Events.emSlug event
                      detailLink = Links.linkURI $ dashboardEventsLinks.detail eventId eventSlug
                      detailUrl = [i|/#{detailLink}|] :: Text
                      banner = BannerParams Success "Event Created" "Your event has been created successfully."
                      redirectUrl = buildRedirectUrl detailUrl banner
                  pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)
                _ -> do
                  Log.logInfo "Failed to fetch event" (Aeson.object ["eventId" .= eventId])
                  let banner = BannerParams Error "Error" "Event created but failed to load details."
                  pure $ Servant.noHeader (redirectWithBanner [i|/#{dashboardEventsListUrl}|] banner)
