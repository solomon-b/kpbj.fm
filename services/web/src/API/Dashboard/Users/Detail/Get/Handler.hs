{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Detail.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Detail.Get.Templates.Page (template)
import API.Links (dashboardLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Has (getter)
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | All data needed to render the user detail page.
data UserDetailViewData = UserDetailViewData
  { udvUserMetadata :: UserMetadata.Model,
    udvSidebarShows :: [Shows.Model],
    udvSelectedShow :: Maybe Shows.Model,
    udvBackend :: StorageBackend,
    udvTargetUser :: User.Model,
    udvTargetMetadata :: UserMetadata.Model,
    udvHostedShows :: [Shows.Model],
    udvRecentEpisodes :: [Episodes.Model]
  }

-- | Business logic: fetch user and activity data.
action ::
  User.Model ->
  UserMetadata.Model ->
  User.Id ->
  ExceptT HandlerError AppM UserDetailViewData
action user userMetadata targetUserId = do
  -- 1. Get storage backend for avatar URLs
  backend <- asks getter

  -- 2. Fetch shows for sidebar
  sidebarShows <- lift $ fetchShowsForUser user userMetadata
  let selectedShow = listToMaybe sidebarShows

  -- 3. Fetch target user and their metadata
  (targetUser, targetMetadata) <- fetchTargetUserOrNotFound targetUserId

  -- 4. Fetch additional info: shows they host, episodes they created
  (hostedShows, recentEpisodes) <- lift $ fetchUserActivity targetUserId

  pure
    UserDetailViewData
      { udvUserMetadata = userMetadata,
        udvSidebarShows = sidebarShows,
        udvSelectedShow = selectedShow,
        udvBackend = backend,
        udvTargetUser = targetUser,
        udvTargetMetadata = targetMetadata,
        udvHostedShows = hostedShows,
        udvRecentEpisodes = recentEpisodes
      }

handler ::
  User.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler targetUserId cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "User detail" dashboardLinks.home $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata targetUserId
    lift $ renderDashboardTemplate hxRequest vd.udvUserMetadata vd.udvSidebarShows vd.udvSelectedShow NavUsers Nothing Nothing (template vd.udvBackend vd.udvTargetUser vd.udvTargetMetadata vd.udvHostedShows vd.udvRecentEpisodes)

-- | Fetch shows based on user role (admins see all, staff see their assigned shows)
fetchShowsForUser ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuery Shows.getAllActiveShows
    else fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))

-- | Fetch target user and their metadata, or throw NotFound
fetchTargetUserOrNotFound ::
  User.Id ->
  ExceptT HandlerError AppM (User.Model, UserMetadata.Model)
fetchTargetUserOrNotFound targetUserId = do
  userDataResult <- execTransaction $ do
    maybeTargetUser <- HT.statement () (User.getUser targetUserId)
    maybeTargetMetadata <- HT.statement () (UserMetadata.getUserMetadata targetUserId)
    pure (maybeTargetUser, maybeTargetMetadata)

  case userDataResult of
    Left err -> throwDatabaseError err
    Right (Nothing, _) -> throwNotFound "User"
    Right (_, Nothing) -> throwNotFound "User"
    Right (Just targetUser, Just targetMetadata) -> pure (targetUser, targetMetadata)

-- | Fetch user activity (shows they host, episodes they created)
fetchUserActivity ::
  User.Id ->
  AppM ([Shows.Model], [Episodes.Model])
fetchUserActivity targetUserId = do
  additionalData <- execTransaction $ do
    shows' <- HT.statement () (Shows.getShowsForUser targetUserId)
    episodes <- HT.statement () (Episodes.getEpisodesByUser targetUserId 10 0)
    pure (shows', episodes)

  case additionalData of
    Left _err -> do
      Log.logInfo "Failed to fetch user activity from database" ()
      pure ([], [])
    Right result -> pure result
