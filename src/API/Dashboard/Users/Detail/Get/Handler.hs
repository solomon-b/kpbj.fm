{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Detail.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Detail.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardUsersLinks, userLinks)
import API.Types (DashboardUsersRoutes (..), Routes (..), UserRoutes (..))
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
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.Episodes qualified as Episodes
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

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

dashboardUsersGetUrl :: Links.URI
dashboardUsersGetUrl = Links.linkURI $ dashboardUsersLinks.list Nothing Nothing Nothing Nothing

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
  User.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer targetUserId cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      let banner = BannerParams Error "Login Required" "You must be logged in to access this page."
      pure $ redirectWithBanner [i|/#{userLoginGetUrl}|] banner
    Just (_user, userMetadata)
      | not (UserMetadata.isAdmin userMetadata.mUserRole) || isSuspended userMetadata -> do
          let banner = BannerParams Error "Admin Access Required" "You do not have permission to access this page."
          pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
    Just (user, userMetadata) -> do
      -- Fetch shows for sidebar (admins see all, staff see their assigned shows)
      showsResult <-
        if UserMetadata.isAdmin userMetadata.mUserRole
          then execQuerySpan Shows.getAllActiveShows
          else execQuerySpan (Shows.getShowsForUser (User.mId user))
      let allShows = fromRight [] showsResult
          selectedShow = listToMaybe allShows

      -- Fetch target user and their metadata
      userDataResult <- execTransactionSpan $ do
        maybeTargetUser <- HT.statement () (User.getUser targetUserId)
        maybeTargetMetadata <- HT.statement () (UserMetadata.getUserMetadata targetUserId)
        pure (maybeTargetUser, maybeTargetMetadata)

      case userDataResult of
        Left _err -> do
          Log.logInfo "Failed to fetch user from database" ()
          let banner = BannerParams Error "Error" "Failed to load user. Please try again."
          pure $ redirectWithBanner [i|/#{dashboardUsersGetUrl}|] banner
        Right (Nothing, _) -> do
          let banner = BannerParams Warning "User Not Found" "The user you are looking for does not exist."
          pure $ redirectWithBanner [i|/#{dashboardUsersGetUrl}|] banner
        Right (_, Nothing) -> do
          let banner = BannerParams Warning "User Not Found" "The user you are looking for does not exist."
          pure $ redirectWithBanner [i|/#{dashboardUsersGetUrl}|] banner
        Right (Just targetUser, Just targetMetadata) -> do
          -- Fetch additional info: shows they host, episodes they created
          additionalData <- execTransactionSpan $ do
            shows' <- HT.statement () (Shows.getShowsForUser targetUserId)
            episodes <- HT.statement () (Episodes.getEpisodesByUser targetUserId 10 0)
            pure (shows', episodes)

          case additionalData of
            Left _err -> do
              Log.logInfo "Failed to fetch user activity from database" ()
              renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavUsers Nothing Nothing (template targetUser targetMetadata [] [])
            Right (shows', episodes) ->
              renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavUsers Nothing Nothing (template targetUser targetMetadata shows' episodes)
