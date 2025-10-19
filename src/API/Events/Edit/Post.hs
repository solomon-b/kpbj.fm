{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Events.Edit.Post where

--------------------------------------------------------------------------------

import API.Events.Edit.Post.Templates.Error (errorTemplate, forbiddenTemplate, notFoundTemplate, unauthorizedTemplate)
import API.Events.Edit.Post.Templates.Success (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.EventTags qualified as EventTags
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /events/:slug/edit"
    ( "events"
        :> Servant.Capture "slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.ReqBody '[Servant.FormUrlEncoded] EventEditForm
        :> Servant.Post '[HTML] (Lucid.Html ())
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
    eefTags :: [Text]
  }
  deriving (Show)

instance FromForm EventEditForm where
  fromForm :: Form.Form -> Either Text EventEditForm
  fromForm form = do
    title <- Form.parseUnique "title" form
    description <- Form.parseUnique "description" form
    startsAt <- Form.parseUnique "starts_at" form
    endsAt <- Form.parseUnique "ends_at" form
    locationName <- Form.parseUnique "location_name" form
    locationAddress <- Form.parseUnique "location_address" form
    status <- Form.parseUnique "status" form
    tags <- Form.parseMaybe "tags" form

    pure
      EventEditForm
        { eefTitle = title,
          eefDescription = description,
          eefStartsAt = startsAt,
          eefEndsAt = endsAt,
          eefLocationName = locationName,
          eefLocationAddress = locationAddress,
          eefStatus = status,
          eefTags = parseTags $ fromMaybe "" tags
        }

-- | Parse comma-separated tags
parseTags :: Text -> [Text]
parseTags tagText =
  filter (not . Text.null) $
    map Text.strip $
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
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  EventEditForm ->
  m (Lucid.Html ())
handler _tracer slug cookie (foldHxReq -> hxRequest) editForm = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized event edit attempt" slug
      renderTemplate hxRequest Nothing unauthorizedTemplate
    Just (user, userMetadata) -> do
      mResult <- execTransactionSpan $ runMaybeT $ do
        event <- MaybeT $ HT.statement () (Events.getEventBySlug slug)
        oldTags <- lift $ HT.statement () (Events.getEventTags event.emId)
        MaybeT $ pure $ Just (event, oldTags)

      case mResult of
        Left err -> do
          Log.logAttention "getEventBySlug execution error" (show err)
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo_ $ "No event with slug: '" <> display slug <> "'"
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right (Just (event, oldTags)) ->
          if event.emAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole
            then updateEvent hxRequest userMetadata event oldTags editForm
            else do
              Log.logInfo "User attempted to edit event they don't own" event.emId
              renderTemplate hxRequest (Just userMetadata) forbiddenTemplate

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
  m (Lucid.Html ())
updateEvent hxRequest userMetadata event oldTags editForm = do
  -- Parse and validate form data
  case (parseStatus (eefStatus editForm), parseDateTime (eefStartsAt editForm), parseDateTime (eefEndsAt editForm)) of
    (Nothing, _, _) -> do
      Log.logInfo "Invalid status in event edit form" (eefStatus editForm)
      renderTemplate hxRequest (Just userMetadata) (errorTemplate "Invalid event status value.")
    (_, Nothing, _) -> do
      Log.logInfo "Invalid starts_at datetime in event edit form" (eefStartsAt editForm)
      renderTemplate hxRequest (Just userMetadata) (errorTemplate "Invalid start date/time format.")
    (_, _, Nothing) -> do
      Log.logInfo "Invalid ends_at datetime in event edit form" (eefEndsAt editForm)
      renderTemplate hxRequest (Just userMetadata) (errorTemplate "Invalid end date/time format.")
    (Just parsedStatus, Just parsedStartsAt, Just parsedEndsAt) -> do
      let newSlug = Slug.mkSlug (eefTitle editForm)
          updateData =
            Events.Insert
              { Events.eiTitle = eefTitle editForm,
                Events.eiSlug = newSlug,
                Events.eiDescription = eefDescription editForm,
                Events.eiStartsAt = parsedStartsAt,
                Events.eiEndsAt = parsedEndsAt,
                Events.eiLocationName = eefLocationName editForm,
                Events.eiLocationAddress = eefLocationAddress editForm,
                Events.eiStatus = parsedStatus,
                Events.eiAuthorId = event.emAuthorId
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
          renderTemplate hxRequest (Just userMetadata) (errorTemplate "Database error occurred. Please try again.")
        Right Nothing -> do
          Log.logInfo "Event update returned Nothing" event.emId
          renderTemplate hxRequest (Just userMetadata) (errorTemplate "Failed to update event. Please try again.")
        Right (Just _) -> do
          Log.logInfo "Successfully updated event" event.emId
          renderTemplate hxRequest (Just userMetadata) (template newSlug)

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
