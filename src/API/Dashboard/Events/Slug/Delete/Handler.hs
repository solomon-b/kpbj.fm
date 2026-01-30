{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Delete.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (dashboardEventsLinks, rootLink)
import API.Types (DashboardEventsRoutes (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad (unless)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler eventId _eventSlug cookie =
  handleBannerErrors "Event delete" $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to delete events." userMetadata

    -- 2. Fetch event
    event <- fetchEvent eventId

    -- 3. Check authorization: must be staff/admin or the creator
    let isAuthorized = event.emAuthorId == user.mId || UserMetadata.isStaffOrHigher userMetadata.mUserRole
    unless isAuthorized $
      throwNotAuthorized "You don't have permission to delete this event." (Just userMetadata.mUserRole)

    -- 4. Delete the event
    deleteEvent event

--------------------------------------------------------------------------------
-- Helpers

fetchEvent ::
  Events.Id ->
  AppM Events.Model
fetchEvent eventId =
  execQuery (Events.getEventById eventId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Event"
    Right (Just event) -> pure event

deleteEvent ::
  Events.Model ->
  AppM (Lucid.Html ())
deleteEvent event =
  execQuery (Events.deleteEvent event.emId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Event"
    Right (Just _) -> do
      Log.logInfo "Event deleted successfully" event.emId
      -- Redirect back to the events list with success banner
      let banner = BannerParams Success "Event Deleted" "The event has been deleted successfully."
          eventsUrl = rootLink $ dashboardEventsLinks.list Nothing
      pure $ redirectWithBanner eventsUrl banner
