{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Detail.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Detail.Get.Templates.Page (template)
import API.Links (dashboardLinks)
import API.Types
import App.Auth.Roles (RoleAuth (..), Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has (getter)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execTransaction)
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
    udvBackend :: StorageBackend,
    udvTargetUser :: User.Model,
    udvTargetMetadata :: UserMetadata.Model,
    udvHostedShows :: [Shows.Model],
    udvRecentEpisodes :: [Episodes.Model]
  }

-- | Business logic: fetch user and activity data.
action ::
  UserMetadata.Model ->
  User.Id ->
  ExceptT HandlerError AppM UserDetailViewData
action userMetadata targetUserId = do
  -- 1. Get storage backend for avatar URLs
  backend <- asks getter

  -- 2. Fetch target user and their metadata
  (targetUser, targetMetadata) <- fetchTargetUserOrNotFound targetUserId

  -- 3. Fetch additional info: shows they host, episodes they created
  (hostedShows, recentEpisodes) <- lift $ fetchUserActivity targetUserId

  pure
    UserDetailViewData
      { udvUserMetadata = userMetadata,
        udvBackend = backend,
        udvTargetUser = targetUser,
        udvTargetMetadata = targetMetadata,
        udvHostedShows = hostedShows,
        udvRecentEpisodes = recentEpisodes
      }

handler ::
  User.Id ->
  Satisfies 'UserMetadata.Staff RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler targetUserId (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "User detail" dashboardLinks.home $ do
    vd <- action auth.raMetadata targetUserId
    lift $ renderDashboardTemplate hxRequest vd.udvUserMetadata NoShow NavUsers Nothing Nothing (template vd.udvBackend vd.udvTargetUser vd.udvTargetMetadata vd.udvHostedShows vd.udvRecentEpisodes)

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
