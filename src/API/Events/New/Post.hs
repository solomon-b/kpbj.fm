{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Events.New.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventGetLink, eventsGetLink, eventsNewGetLink)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Foldable (traverse_)
import Data.Has (Has)
import Data.List (find)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.EventTags qualified as EventTags
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

--------------------------------------------------------------------------------

-- URL helpers
eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsGetLink Nothing Nothing

eventsNewGetUrl :: Links.URI
eventsNewGetUrl = Links.linkURI eventsNewGetLink

eventGetUrl :: Events.Id -> Slug -> Links.URI
eventGetUrl eventId slug = Links.linkURI $ eventGetLink eventId slug

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /events/new"
    ( "events"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.ReqBody '[Servant.FormUrlEncoded] NewEventForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Success template after event creation
successTemplate :: Events.Model -> Lucid.Html ()
successTemplate event = do
  Lucid.div_ [Lucid.class_ "bg-green-100 border-2 border-green-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-green-800"] "✓ Event Created Successfully!"
    Lucid.p_ [Lucid.class_ "mb-6"] $ do
      "Your event \""
      Lucid.strong_ $ Lucid.toHtml event.emTitle
      "\" has been "
      case event.emStatus of
        Events.Published -> "published and is now live."
        Events.Draft -> "saved as a draft."

    Lucid.div_ [Lucid.class_ "space-x-4"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{eventGetUrl (Events.emId event) (Events.emSlug event)}|],
          hxGet_ [i|/#{eventGetUrl (Events.emId event) (Events.emSlug event)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700 inline-block"
        ]
        "VIEW EVENT"
      Lucid.a_
        [ Lucid.href_ [i|/#{eventsGetUrl}|],
          hxGet_ [i|/#{eventsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
        ]
        "← BACK TO EVENTS"
      Lucid.a_
        [ Lucid.href_ [i|/#{eventsNewGetUrl}|],
          hxGet_ [i|/#{eventsNewGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-green-600 text-white px-6 py-3 font-bold hover:bg-green-700 inline-block"
        ]
        "CREATE ANOTHER EVENT"

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
        [ Lucid.href_ [i|/#{eventsNewGetUrl}|],
          hxGet_ [i|/#{eventsNewGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
        ]
        "← TRY AGAIN"

-- | Template for unauthorized access
notAuthorizedTemplate :: Lucid.Html ()
notAuthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "Only Staff and Admin users can create events."
    Lucid.div_ [Lucid.class_ "space-x-4"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{eventsGetUrl}|],
          hxGet_ [i|/#{eventsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
        ]
        "← BACK TO EVENTS"

--------------------------------------------------------------------------------

-- | Parse and sanitize comma-separated tags
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map (Sanitize.sanitizePlainText . Text.strip) $
      Text.splitOn "," tagText

-- Form data structure
data NewEventForm = NewEventForm
  { nefTitle :: Text,
    nefTags :: Text,
    nefDescription :: Text,
    nefStartsAt :: Text,
    nefEndsAt :: Text,
    nefLocationName :: Text,
    nefLocationAddress :: Text,
    nefStatus :: Text
  }
  deriving (Show, Eq)

instance FromForm NewEventForm where
  fromForm f =
    NewEventForm
      <$> parseUnique "title" f
      <*> parseUnique "tags" f
      <*> parseUnique "description" f
      <*> parseUnique "starts_at" f
      <*> parseUnique "ends_at" f
      <*> parseUnique "location_name" f
      <*> parseUnique "location_address" f
      <*> parseUnique "status" f

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

validateTitle :: Text -> Either [Text] Text
validateTitle title
  | Text.null (Text.strip title) = Left ["Event title cannot be empty"]
  | Text.length title < 3 = Left ["Event title must be at least 3 characters long"]
  | Text.length title > 200 = Left ["Event title cannot exceed 200 characters"]
  | otherwise = Right (Text.strip title)

validateDescription :: Text -> Either [Text] Text
validateDescription description
  | Text.null (Text.strip description) = Left ["Event description cannot be empty"]
  | Text.length description < 10 = Left ["Event description must be at least 10 characters long"]
  | Text.length description > 5000 = Left ["Event description cannot exceed 5000 characters"]
  | otherwise = Right (Text.strip description)

validateDateTime :: Text -> Text -> Either [Text] UTCTime
validateDateTime fieldName dateTimeStr
  | Text.null (Text.strip dateTimeStr) = Left [fieldName <> " cannot be empty"]
  | otherwise = case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" (Text.unpack dateTimeStr) of
      Nothing -> Left [fieldName <> " is not in a valid format"]
      Just utcTime -> Right utcTime

validateLocationName :: Text -> Either [Text] Text
validateLocationName locationName
  | Text.null (Text.strip locationName) = Left ["Venue name cannot be empty"]
  | Text.length locationName < 2 = Left ["Venue name must be at least 2 characters long"]
  | Text.length locationName > 200 = Left ["Venue name cannot exceed 200 characters"]
  | otherwise = Right (Text.strip locationName)

validateLocationAddress :: Text -> Either [Text] Text
validateLocationAddress locationAddress
  | Text.null (Text.strip locationAddress) = Left ["Address cannot be empty"]
  | Text.length locationAddress < 5 = Left ["Address must be at least 5 characters long"]
  | Text.length locationAddress > 300 = Left ["Address cannot exceed 300 characters"]
  | otherwise = Right (Text.strip locationAddress)

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
  m (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) form = do
  getUserInfo cookie >>= \case
    Just (user, userMetadata) | UserMetadata.isStaffOrHigher userMetadata.mUserRole ->
      validateForm hxRequest user userMetadata form $ \eventInsert ->
        insertEvent hxRequest userMetadata eventInsert $ \eventId -> do
          Log.logInfo "Event created successfully" eventId
          traverse_ (addTag eventId) (parseTags (nefTags form))

          fetchEvent hxRequest userMetadata eventId
    _ -> do
      Log.logInfo "Unauthorized access to event creation form" ()
      renderTemplate hxRequest Nothing notAuthorizedTemplate

validateForm ::
  ( Log.MonadLog m,
    MonadCatch m
  ) =>
  HxRequest ->
  User.Model ->
  UserMetadata.Model ->
  NewEventForm ->
  (Events.Insert -> m (Lucid.Html ())) ->
  m (Lucid.Html ())
validateForm hxRequest user userMetadata form k =
  case validateEventForm form of
    Left validationError -> do
      let errorMsg = Sanitize.displayContentValidationError validationError
      Log.logInfo "Event creation failed validation" errorMsg
      renderTemplate hxRequest (Just userMetadata) (errorTemplate [errorMsg])
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
                Events.eiAuthorId = user.mId
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
  Events.Insert ->
  (Events.Id -> m (Lucid.Html ())) ->
  m (Lucid.Html ())
insertEvent hxRequest userMetadata eventInsert k =
  execQuerySpan (Events.insertEvent eventInsert) >>= \case
    Left _err -> do
      Log.logInfo "Failed to create event in database" ()
      renderTemplate hxRequest (Just userMetadata) (errorTemplate ["Database error occurred. Please try again."])
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
              -- TODO: Handle tag creation failures
              pure ()
    Left _ ->
      -- TODO: Handle tag lookup failures
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
  Events.Id ->
  m (Lucid.Html ())
fetchEvent hxRequest userMetadata eventId =
  execQuerySpan (Events.getEventById eventId) >>= \case
    Right (Just event) -> do
      renderTemplate hxRequest (Just userMetadata) (successTemplate event)
    _ -> do
      Log.logInfo "Failed to fetc event" (Aeson.object ["eventId" .= eventId])
      renderTemplate hxRequest (Just userMetadata) (errorTemplate ["Database error occurred. Please try again."])
