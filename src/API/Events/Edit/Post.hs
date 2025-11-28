{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Events.Edit.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventGetLink)
import API.Events.Edit.Post.Templates.Error (errorTemplate, forbiddenTemplate, notFoundTemplate, unauthorizedTemplate)
import API.Events.Event.Get.Templates.Page qualified as DetailPage
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as Aeson
import Data.Foldable (fold, traverse_)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.EventTags qualified as EventTags
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (stripStorageRoot, uploadEventPosterImage)
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /events/:id/:slug/edit"
    ( "events"
        :> Servant.Capture "id" Events.Id
        :> Servant.Capture "slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> MultipartForm Mem EventEditForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
    )

--------------------------------------------------------------------------------

-- | Form data for event editing
data EventEditForm = EventEditForm
  { eefTitle :: Text,
    eefDescription :: Text,
    eefStartsAt :: Text,
    eefEndsAt :: Text,
    eefLocationName :: Text,
    eefLocationAddress :: Text,
    eefStatus :: Text,
    eefTags :: [Text],
    eefPosterImage :: Maybe (FileData Mem)
  }
  deriving (Show)

instance FromMultipart Mem EventEditForm where
  fromMultipart multipartData =
    EventEditForm
      <$> lookupInput "title" multipartData
      <*> lookupInput "description" multipartData
      <*> lookupInput "starts_at" multipartData
      <*> lookupInput "ends_at" multipartData
      <*> lookupInput "location_name" multipartData
      <*> lookupInput "location_address" multipartData
      <*> lookupInput "status" multipartData
      <*> pure (parseTags $ fold $ either (const Nothing) Just (lookupInput "tags" multipartData))
      <*> pure (fileDataToNothing $ either (const Nothing) Just (lookupFile "poster_image" multipartData))
    where
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

-- | Parse comma-separated tags
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map (Sanitize.sanitizePlainText . Text.strip) $
      Text.splitOn "," tagText

-- | Parse event status from text
parseStatus :: Text -> Maybe Events.Status
parseStatus "published" = Just Events.Published
parseStatus "draft" = Just Events.Draft
parseStatus _ = Nothing

-- | Parse datetime from HTML5 datetime-local format
parseDateTime :: Text -> Maybe UTCTime
parseDateTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" . Text.unpack

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
  Maybe HxRequest ->
  EventEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handler _tracer eventId _slug cookie (foldHxReq -> hxRequest) editForm = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized event edit attempt" eventId
      Servant.noHeader <$> renderTemplate hxRequest Nothing unauthorizedTemplate
    Just (user, userMetadata) -> do
      mResult <- execTransactionSpan $ runMaybeT $ do
        event <- MaybeT $ HT.statement () (Events.getEventById eventId)
        oldTags <- lift $ HT.statement () (Events.getEventTags event.emId)
        MaybeT $ pure $ Just (event, oldTags)

      case mResult of
        Left err -> do
          Log.logAttention "getEventById execution error" (show err)
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo "No event found with id" eventId
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right (Just (event, oldTags)) ->
          if event.emAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole
            then updateEvent hxRequest userMetadata event oldTags editForm
            else do
              Log.logInfo "User attempted to edit event they don't own" event.emId
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) forbiddenTemplate

updateEvent ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  HxRequest ->
  UserMetadata.Model ->
  Events.Model ->
  [EventTags.Model] ->
  EventEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
updateEvent hxRequest userMetadata event oldTags editForm = do
  -- Parse and validate form data
  case (parseStatus (eefStatus editForm), parseDateTime (eefStartsAt editForm), parseDateTime (eefEndsAt editForm)) of
    (Nothing, _, _) -> do
      Log.logInfo "Invalid status in event edit form" (eefStatus editForm)
      Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Invalid event status value.")
    (_, Nothing, _) -> do
      Log.logInfo "Invalid starts_at datetime in event edit form" (eefStartsAt editForm)
      Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Invalid start date/time format.")
    (_, _, Nothing) -> do
      Log.logInfo "Invalid ends_at datetime in event edit form" (eefEndsAt editForm)
      Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Invalid end date/time format.")
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
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate errorMsg)
        (_, Left descError, _, _) -> do
          let errorMsg = Sanitize.displayContentValidationError descError
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate errorMsg)
        (_, _, Left nameError, _) -> do
          let errorMsg = Sanitize.displayContentValidationError nameError
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate errorMsg)
        (_, _, _, Left addrError) -> do
          let errorMsg = Sanitize.displayContentValidationError addrError
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate errorMsg)
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
                Right result -> do
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

          -- Update the event and tags in a transaction
          mUpdateResult <- execTransactionSpan $ runMaybeT $ do
            -- Update event
            _ <- MaybeT $ HT.statement () (Events.updateEvent event.emId updateData)
            -- Remove old tags
            lift $ traverse_ (\tag -> HT.statement () (Events.removeTagFromEvent event.emId tag.etmId)) oldTags
            -- Add new tags
            lift $ updateEventTags event.emId editForm
            MaybeT $ pure $ Just ()

          case mUpdateResult of
            Left err -> do
              Log.logInfo "Failed to update event" (event.emId, show err)
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Database error occurred. Please try again.")
            Right Nothing -> do
              Log.logInfo "Event update returned Nothing" event.emId
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Failed to update event. Please try again.")
            Right (Just _) -> do
              Log.logInfo "Successfully updated event" event.emId
              -- Fetch updated event data for detail page
              updatedEventData <- execTransactionSpan $ runMaybeT $ do
                updatedEvent <- MaybeT $ HT.statement () (Events.getEventById event.emId)
                author <- MaybeT $ HT.statement () (UserMetadata.getUserMetadata updatedEvent.emAuthorId)
                newTags <- lift $ HT.statement () (Events.getEventTags event.emId)
                pure (updatedEvent, author, newTags)

              case updatedEventData of
                Left _err ->
                  Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Event updated but failed to load details.")
                Right Nothing ->
                  Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Event updated but not found.")
                Right (Just (updatedEvent, author, newTags)) -> do
                  let detailUrl = Links.linkURI $ eventGetLink event.emId newSlug
                      banner = renderBanner Success "Event Updated" "Your event has been updated and saved."
                  html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                    IsHxRequest -> do
                      DetailPage.template updatedEvent newTags author
                      banner
                    IsNotHxRequest -> do
                      banner
                      DetailPage.template updatedEvent newTags author
                  pure $ Servant.addHeader [i|/#{detailUrl}|] html

-- | Update tags for an event (add new ones)
updateEventTags ::
  Events.Id ->
  EventEditForm ->
  HT.Transaction ()
updateEventTags eventId form = do
  let newTags = eefTags form
  unless (null newTags) $
    traverse_ (createOrAssociateTag eventId) newTags

-- | Create a new tag or associate an existing one with an event
createOrAssociateTag ::
  Events.Id ->
  Text ->
  HT.Transaction ()
createOrAssociateTag eventId tagName = do
  -- Check if tag already exists
  mExistingTags <- HT.statement () EventTags.getAllEventTags
  case filter (\t -> t.etmName == tagName) mExistingTags of
    (existingTag : _) -> do
      -- If tag exists, associate it
      void $ HT.statement () (Events.assignTagToEvent eventId (EventTags.etmId existingTag))
    [] -> do
      -- Otherwise, create new tag and associate it
      newTagId <- HT.statement () (EventTags.insertEventTag (EventTags.Insert tagName))
      void $ HT.statement () (Events.assignTagToEvent eventId newTagId)
