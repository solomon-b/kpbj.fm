{-# LANGUAGE OverloadedRecordDot #-}

module API.Events.Delete where

--------------------------------------------------------------------------------

import App.Common (getUserInfo)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Base qualified as LucidBase
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "DELETE /events/:event_id/:event_slug"
    ( "events"
        :> Servant.Capture "event_id" Events.Id
        :> Servant.Capture "event_slug" Slug
        :> Servant.Header "Cookie" Cookie
        :> Servant.Delete '[HTML] (Lucid.Html ())
    )

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
  m (Lucid.Html ())
handler _tracer eventId _eventSlug cookie = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo_ "No user session"
      pure $ renderSimpleErrorBanner "You must be logged in to delete events."
    Just (user, userMetadata) -> do
      execQuerySpan (Events.getEventById eventId) >>= \case
        Left err -> do
          Log.logInfo "Delete failed: Failed to fetch event" (Aeson.object ["error" .= show err])
          pure $ renderSimpleErrorBanner "Database error. Please try again or contact support."
        Right Nothing -> do
          Log.logInfo "Delete failed: Event not found" (Aeson.object ["eventId" .= eventId])
          pure $ renderSimpleErrorBanner "Event not found."
        Right (Just event) -> do
          -- Check authorization: must be staff/admin or the creator
          let isCreator = event.emAuthorId == user.mId
              isStaffOrAdmin = UserMetadata.isStaffOrHigher userMetadata.mUserRole

          if isCreator || isStaffOrAdmin
            then deleteEvent event
            else do
              Log.logInfo "Delete failed: Not authorized" (Aeson.object ["userId" .= user.mId, "eventId" .= event.emId])
              pure $ renderSimpleErrorBanner "You don't have permission to delete this event."

-- Helper for simple error banners
renderSimpleErrorBanner :: Text -> Lucid.Html ()
renderSimpleErrorBanner errorMsg =
  Lucid.div_
    [ Lucid.id_ "error-banner-container",
      LucidBase.makeAttributes "hx-swap-oob" "true"
    ]
    $ do
      Lucid.div_
        [ Lucid.id_ "error-banner",
          Lucid.class_ "bg-red-100 border-2 border-red-600 p-4 mb-6 w-full"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
            Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
              Lucid.span_ [Lucid.class_ "text-2xl"] "⚠️"
              Lucid.div_ $ do
                Lucid.h3_ [Lucid.class_ "font-bold text-red-800"] "Delete Failed"
                Lucid.p_ [Lucid.class_ "text-sm text-red-700"] $ Lucid.toHtml errorMsg
            Lucid.button_
              [ Lucid.onclick_ "this.closest('#error-banner').remove()",
                Lucid.class_ "text-red-600 hover:text-red-800 font-bold text-xl"
              ]
              "✕"

deleteEvent ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Events.Model ->
  m (Lucid.Html ())
deleteEvent event = do
  execQuerySpan (Events.deleteEvent event.emId) >>= \case
    Left err -> do
      Log.logInfo "Delete failed: Database error" (Aeson.object ["error" .= show err, "eventId" .= event.emId])
      pure $ renderSimpleErrorBanner "Failed to delete event due to a database error."
    Right Nothing -> do
      Log.logInfo "Delete failed: Event not found during delete" (Aeson.object ["eventId" .= event.emId])
      pure $ renderSimpleErrorBanner "Event not found during delete operation."
    Right (Just _) -> do
      Log.logInfo "Event deleted successfully" (Aeson.object ["eventId" .= event.emId])
      -- Return empty response so the event card gets removed
      pure $ Lucid.toHtmlRaw ("" :: Text)
