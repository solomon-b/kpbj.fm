{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.Slug.Edit.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Edit.Get.Templates.Form (template)
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
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Has (Has)
import Data.Maybe (listToMaybe)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardEventsGetUrl :: Links.URI
dashboardEventsGetUrl = Links.linkURI $ dashboardEventsLinks.list Nothing

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

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
handler _tracer eventId _urlSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to event edit" ()
      let banner = BannerParams Error "Access Denied" "You must be logged in to edit events."
      pure $ redirectWithBanner [i|/#{userLoginGetUrl}|] banner
    Just (_user, userMetadata)
      | not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) || isSuspended userMetadata -> do
          let banner = BannerParams Error "Staff Access Required" "You do not have permission to edit events."
          pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
    Just (user, userMetadata) -> do
      -- Fetch shows for sidebar (admins see all, staff see their assigned shows)
      showsResult <-
        if UserMetadata.isAdmin userMetadata.mUserRole
          then execQuerySpan Shows.getAllActiveShows
          else execQuerySpan (Shows.getShowsForUser (User.mId user))
      let allShows = either (const []) id showsResult
          selectedShow = listToMaybe allShows

      mResult <- execTransactionSpan $ runMaybeT $ do
        event <- MaybeT $ HT.statement () (Events.getEventById eventId)
        tags <- lift $ HT.statement () (Events.getEventTags event.emId)
        MaybeT $ pure $ Just (event, tags)

      case mResult of
        Left err -> do
          Log.logAttention "getEventById execution error" (show err)
          let banner = BannerParams Warning "Event Not Found" "The event you're trying to edit doesn't exist."
          pure $ redirectWithBanner [i|/#{dashboardEventsGetUrl}|] banner
        Right Nothing -> do
          Log.logInfo "No event found with id" eventId
          let banner = BannerParams Warning "Event Not Found" "The event you're trying to edit doesn't exist."
          pure $ redirectWithBanner [i|/#{dashboardEventsGetUrl}|] banner
        Right (Just (event, tags)) -> do
          -- Check authorization: must be staff/admin or the creator
          if event.emAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole
            then do
              Log.logInfo "Authorized user accessing event edit form" event.emId
              let editTemplate = template event tags userMetadata
              renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavEvents Nothing Nothing editTemplate
            else do
              Log.logInfo "User tried to edit event they don't own" event.emId
              let banner = BannerParams Error "Access Denied" "You can only edit events you created or have staff permissions."
              pure $ redirectWithBanner [i|/#{dashboardEventsGetUrl}|] banner
