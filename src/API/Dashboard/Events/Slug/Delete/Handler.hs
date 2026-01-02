{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Delete.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (dashboardEventsLinks, rootLink)
import API.Types (DashboardEventsRoutes (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

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
handler _tracer eventId _eventSlug cookie =
  handleBannerErrors "Event delete" $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to delete events." userMetadata

    -- 2. Fetch event
    event <- fetchEvent eventId

    -- 3. Check authorization: must be staff/admin or the creator
    let isAuthorized = event.emAuthorId == user.mId || UserMetadata.isStaffOrHigher userMetadata.mUserRole
    unless isAuthorized $
      throwNotAuthorized "You don't have permission to delete this event."

    -- 4. Delete the event
    deleteEvent event

--------------------------------------------------------------------------------
-- Helpers

fetchEvent ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m,
    MonadThrow m
  ) =>
  Events.Id ->
  m Events.Model
fetchEvent eventId =
  execQuerySpan (Events.getEventById eventId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Event"
    Right (Just event) -> pure event

deleteEvent ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m,
    MonadThrow m
  ) =>
  Events.Model ->
  m (Lucid.Html ())
deleteEvent event =
  execQuerySpan (Events.deleteEvent event.emId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Event"
    Right (Just _) -> do
      Log.logInfo "Event deleted successfully" event.emId
      -- Redirect back to the events list with success banner
      let banner = BannerParams Success "Event Deleted" "The event has been deleted successfully."
          eventsUrl = rootLink $ dashboardEventsLinks.list Nothing
      pure $ redirectWithBanner eventsUrl banner
