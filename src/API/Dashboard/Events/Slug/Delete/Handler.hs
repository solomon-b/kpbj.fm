{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Events.Slug.Delete.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (dashboardEventsLinks)
import API.Types (DashboardEventsRoutes (..))
import App.Common (getUserInfo)
import Component.Banner (BannerType (..), renderBanner)
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardEventsGetUrl :: Links.URI
dashboardEventsGetUrl = Links.linkURI $ dashboardEventsLinks.list Nothing

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
      pure $ renderBanner Error "Delete Failed" "You must be logged in to delete events."
    Just (_user, userMetadata)
      | not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) || isSuspended userMetadata -> do
          Log.logInfo_ "Delete failed: Not authorized (not staff)"
          pure $ renderBanner Error "Delete Failed" "You do not have permission to delete events."
    Just (user, userMetadata) -> do
      execQuerySpan (Events.getEventById eventId) >>= \case
        Left err -> do
          Log.logInfo "Delete failed: Failed to fetch event" (Aeson.object ["error" .= show err])
          pure $ renderBanner Error "Delete Failed" "Database error. Please try again or contact support."
        Right Nothing -> do
          Log.logInfo "Delete failed: Event not found" (Aeson.object ["eventId" .= eventId])
          pure $ renderBanner Error "Delete Failed" "Event not found."
        Right (Just event)
          | -- Check authorization: must be staff/admin or the creator
            event.emAuthorId /= user.mId && not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) -> do
              Log.logInfo "Delete failed: Not authorized" (Aeson.object ["userId" .= user.mId, "eventId" .= event.emId])
              pure $ renderBanner Error "Delete Failed" "You don't have permission to delete this event."
        Right (Just event) -> do
          execQuerySpan (Events.deleteEvent event.emId) >>= \case
            Left err -> do
              Log.logInfo "Delete failed: Database error" (Aeson.object ["error" .= show err, "eventId" .= event.emId])
              pure $ renderBanner Error "Delete Failed" "Failed to delete event due to a database error."
            Right Nothing -> do
              Log.logInfo "Delete failed: Event not found during delete" (Aeson.object ["eventId" .= event.emId])
              pure $ renderBanner Error "Delete Failed" "Event not found during delete operation."
            Right (Just _) -> do
              Log.logInfo "Event deleted successfully" (Aeson.object ["eventId" .= event.emId])
              -- Redirect back to the events list with success banner
              let banner = BannerParams Success "Event Deleted" "The event has been deleted successfully."
              pure $ redirectWithBanner [i|/#{dashboardEventsGetUrl}|] banner
