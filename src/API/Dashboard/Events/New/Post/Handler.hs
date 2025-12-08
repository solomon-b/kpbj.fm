{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.New.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.New.Post.Route (NewEventForm (..), parseTags)
import API.Dashboard.Events.Slug.Get.Templates.Page qualified as DetailPage
import API.Links (apiLinks, dashboardEventsLinks, userLinks)
import API.Types (DashboardEventsRoutes (..), Routes (..), UserRoutes (..))
import App.Common (getUserInfo, renderDashboardTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Has (Has)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug ()
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.EventTags qualified as EventTags
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (stripStorageRoot, uploadEventPosterImage)
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardEventsNewGetUrl :: Links.URI
dashboardEventsNewGetUrl = Links.linkURI dashboardEventsLinks.newGet

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

--------------------------------------------------------------------------------

-- | Error template for validation failures
errorTemplate :: [Text] -> Lucid.Html ()
errorTemplate errors = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Event Creation Failed"
    Lucid.div_ [Lucid.class_ "mb-6"] $ do
      Lucid.p_ [Lucid.class_ "mb-4 font-bold"] "Please fix the following errors:"
      Lucid.ul_ [Lucid.class_ "list-disc list-inside space-y-2"] $ do
        mapM_ (Lucid.li_ [Lucid.class_ "text-red-700"] . Lucid.toHtml) errors

    Lucid.div_ [Lucid.class_ "text-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{dashboardEventsNewGetUrl}|],
          hxGet_ [i|/#{dashboardEventsNewGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
        ]
        "<- TRY AGAIN"

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
  Maybe HxRequest ->
  NewEventForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handler _tracer cookie (foldHxReq -> hxRequest) form = do
  getUserInfo cookie >>= \case
    Nothing -> do
      let banner = BannerParams Error "Login Required" "You must be logged in to create events."
      pure (Servant.noHeader (redirectWithBanner [i|/#{userLoginGetUrl}|] banner))
    Just (_user, userMetadata)
      | not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) || isSuspended userMetadata -> do
          let banner = BannerParams Error "Staff Access Required" "You do not have permission to create events."
          pure (Servant.noHeader (redirectWithBanner [i|/#{rootGetUrl}|] banner))
    Just (user, userMetadata) -> do
      -- Fetch shows for sidebar
      showsResult <-
        if UserMetadata.isAdmin userMetadata.mUserRole
          then execQuerySpan Shows.getAllActiveShows
          else execQuerySpan (Shows.getShowsForUser (User.mId user))
      let allShows = fromRight [] showsResult
          selectedShow = listToMaybe allShows

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
            Right result -> do
              Log.logInfo "Poster image uploaded successfully" (Aeson.object ["path" Aeson..= uploadResultStoragePath result])
              pure (Just $ stripStorageRoot $ uploadResultStoragePath result)

      validateForm hxRequest user userMetadata allShows selectedShow posterImagePath form $ \eventInsert ->
        insertEvent hxRequest userMetadata allShows selectedShow eventInsert $ \eventId -> do
          Log.logInfo "Event created successfully" eventId
          traverse_ (addTag eventId) (parseTags (nefTags form))

          fetchEvent hxRequest userMetadata allShows selectedShow eventId

validateForm ::
  ( Log.MonadLog m,
    MonadCatch m
  ) =>
  HxRequest ->
  User.Model ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Maybe Shows.Model ->
  Maybe Text ->
  NewEventForm ->
  (Events.Insert -> m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))) ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
validateForm hxRequest user userMetadata allShows selectedShow posterImagePath form k =
  case validateEventForm form of
    Left validationError -> do
      let errorMsg = Sanitize.displayContentValidationError validationError
      Log.logInfo "Event creation failed validation" errorMsg
      Servant.noHeader <$> renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavEvents Nothing Nothing (errorTemplate [errorMsg])
    Right (title, description, startsAt, endsAt, locationName, locationAddress, status) ->
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
       in k eventInsert

insertEvent ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env0 m,
    MonadCatch m,
    Has Tracer env0,
    MonadUnliftIO m
  ) =>
  HxRequest ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Maybe Shows.Model ->
  Events.Insert ->
  (Events.Id -> m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))) ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
insertEvent hxRequest userMetadata allShows selectedShow eventInsert k =
  execQuerySpan (Events.insertEvent eventInsert) >>= \case
    Left _err -> do
      Log.logInfo "Failed to create event in database" ()
      Servant.noHeader <$> renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavEvents Nothing Nothing (errorTemplate ["Database error occurred. Please try again."])
    Right eventId ->
      k eventId

addTag ::
  ( MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env
  ) =>
  Events.Id ->
  Text ->
  m ()
addTag eventId tagName = do
  -- Get all existing tags to check if this one exists
  execQuerySpan EventTags.getAllEventTags >>= \case
    Right allTags -> do
      case find (\tag -> tag.etmName == tagName) allTags of
        Just existingTag -> do
          -- Tag exists, assign it to event
          _ <- execQuerySpan (Events.assignTagToEvent eventId existingTag.etmId)
          pure ()
        Nothing -> do
          -- Tag doesn't exist, create it first then assign
          execQuerySpan (EventTags.insertEventTag (EventTags.Insert tagName)) >>= \case
            Right newTagId -> do
              _ <- execQuerySpan (Events.assignTagToEvent eventId newTagId)
              pure ()
            Left _ ->
              pure ()
    Left _ ->
      pure ()

fetchEvent ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  HxRequest ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Maybe Shows.Model ->
  Events.Id ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
fetchEvent hxRequest userMetadata allShows selectedShow eventId =
  execQuerySpan (Events.getEventById eventId) >>= \case
    Right (Just event) -> do
      -- Get tags for the event
      tagsResult <- execQuerySpan (Events.getEventTags eventId)
      let tags = fromRight [] tagsResult
      -- Get author metadata
      authorResult <- execQuerySpan (UserMetadata.getUserMetadata event.emAuthorId)
      let mAuthor = fromRight Nothing authorResult
      let detailUrl = Links.linkURI $ dashboardEventsLinks.detail eventId (Events.emSlug event)
          banner = renderBanner Success "Event Created" "Your event has been created successfully."
      html <- renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavEvents Nothing Nothing $ case hxRequest of
        IsHxRequest -> do
          DetailPage.template event tags mAuthor
          banner
        IsNotHxRequest -> do
          banner
          DetailPage.template event tags mAuthor
      pure $ Servant.addHeader [i|/#{detailUrl}|] html
    _ -> do
      Log.logInfo "Failed to fetch event" (Aeson.object ["eventId" .= eventId])
      Servant.noHeader <$> renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavEvents Nothing Nothing (errorTemplate ["Database error occurred. Please try again."])
