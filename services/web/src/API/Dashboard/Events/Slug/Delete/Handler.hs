{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Delete.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Links (dashboardEventsLinks, rootLink)
import API.Types (DashboardEventsRoutes (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad (unless)
import Control.Monad.Trans.Except (ExceptT)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Business logic: fetch, authorize, delete.
action ::
  User.Model ->
  UserMetadata.Model ->
  Events.Id ->
  ExceptT HandlerError AppM ()
action user userMetadata eventId = do
  -- 1. Fetch event
  event <- fetchEvent eventId

  -- 2. Check authorization: must be staff/admin or the creator
  let isAuthorized = event.emAuthorId == user.mId || UserMetadata.isStaffOrHigher userMetadata.mUserRole
  unless isAuthorized $
    throwNotAuthorized "You don't have permission to delete this event." (Just userMetadata.mUserRole)

  -- 3. Delete the event
  deleteEvent event

-- | Servant handler: thin glue composing action + banner response.
handler ::
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler eventId _eventSlug cookie =
  handleBannerErrors "Event delete" $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to delete events." userMetadata
    action user userMetadata eventId
    let banner = BannerParams Success "Event Deleted" "The event has been deleted successfully."
        eventsUrl = rootLink $ dashboardEventsLinks.list Nothing
    pure $ redirectWithBanner eventsUrl banner

--------------------------------------------------------------------------------
-- Helpers

fetchEvent ::
  Events.Id ->
  ExceptT HandlerError AppM Events.Model
fetchEvent eventId =
  fromMaybeM (throwNotFound "Event") $
    fromRightM throwDatabaseError $
      execQuery (Events.getEventById eventId)

deleteEvent ::
  Events.Model ->
  ExceptT HandlerError AppM ()
deleteEvent event = do
  result <-
    fromRightM throwDatabaseError $
      execQuery (Events.deleteEvent event.emId)
  case result of
    Nothing -> throwNotFound "Event"
    Just _ -> Log.logInfo "Event deleted successfully" event.emId
