{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.Slug.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardEventsLinks, userLinks)
import API.Types (DashboardEventsRoutes (..), Routes (..), UserRoutes (..))
import App.Common (getUserInfo, renderDashboardTemplate)
import Component.Banner (BannerType (..))
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Maybe (listToMaybe)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as Txn
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

dashboardEventsNewGetUrl :: Links.URI
dashboardEventsNewGetUrl = Links.linkURI dashboardEventsLinks.newGet

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
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer eventId _slug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      let banner = BannerParams Error "Login Required" "You must be logged in to access this page."
      pure $ redirectWithBanner [i|/#{userLoginGetUrl}|] banner
    Just (_user, userMetadata)
      | not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) || isSuspended userMetadata -> do
          let banner = BannerParams Error "Staff Access Required" "You do not have permission to access this page."
          pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
    Just (user, userMetadata) -> do
      -- Fetch shows for sidebar (admins see all, staff see their assigned shows)
      showsResult <-
        if UserMetadata.isAdmin userMetadata.mUserRole
          then execQuerySpan Shows.getAllActiveShows
          else execQuerySpan (Shows.getShowsForUser (User.mId user))
      let allShows = fromRight [] showsResult
          selectedShow = listToMaybe allShows

      -- Fetch event with author
      execTransactionSpan (fetchEventData eventId) >>= \case
        Left _err -> do
          Log.logInfo "Failed to fetch event from database" ()
          let banner = BannerParams Error "Error" "Failed to load event. Please try again."
          pure $ redirectWithBanner [i|/#{dashboardEventsGetUrl}|] banner
        Right Nothing -> do
          Log.logInfo "Event not found" eventId
          let banner = BannerParams Error "Not Found" "Event not found."
          pure $ redirectWithBanner [i|/#{dashboardEventsGetUrl}|] banner
        Right (Just (event, mAuthor)) -> do
          let eventTemplate = template event mAuthor
          renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavEvents Nothing (Just actionButton) eventTemplate

-- | Action button for creating new event
actionButton :: Lucid.Html ()
actionButton =
  Lucid.a_
    [ Lucid.href_ [i|/#{dashboardEventsNewGetUrl}|],
      hxGet_ [i|/#{dashboardEventsNewGetUrl}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
    ]
    "New Event"

-- | Fetch event data for detail view
fetchEventData ::
  Events.Id ->
  Txn.Transaction (Maybe (Events.Model, Maybe UserMetadata.Model))
fetchEventData eventId = do
  mEvent <- Txn.statement () (Events.getEventById eventId)
  case mEvent of
    Nothing -> pure Nothing
    Just event -> do
      mAuthor <- Txn.statement () (UserMetadata.getUserMetadata event.emAuthorId)
      pure $ Just (event, mAuthor)
