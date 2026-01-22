{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Detail.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Detail.Get.Templates.Page (template)
import API.Links (dashboardLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Data.Has (getter)
import Data.Either (fromRight)
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  User.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer targetUserId cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "User detail" dashboardLinks.home $ do
    -- 1. Require authentication and admin role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Get storage backend for avatar URLs
    backend <- asks getter

    -- 3. Fetch shows for sidebar
    allShows <- fetchShowsForUser user userMetadata
    let selectedShow = listToMaybe allShows

    -- 4. Fetch target user and their metadata
    (targetUser, targetMetadata) <- fetchTargetUserOrNotFound targetUserId

    -- 5. Fetch additional info: shows they host, episodes they created
    (shows', episodes) <- fetchUserActivity targetUserId

    -- 6. Render page
    renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavUsers Nothing Nothing (template backend targetUser targetMetadata shows' episodes)

-- | Fetch shows based on user role (admins see all, staff see their assigned shows)
fetchShowsForUser ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuerySpan Shows.getAllActiveShows
    else fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))

-- | Fetch target user and their metadata, or throw NotFound
fetchTargetUserOrNotFound ::
  User.Id ->
  AppM (User.Model, UserMetadata.Model)
fetchTargetUserOrNotFound targetUserId = do
  userDataResult <- execTransactionSpan $ do
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
  additionalData <- execTransactionSpan $ do
    shows' <- HT.statement () (Shows.getShowsForUser targetUserId)
    episodes <- HT.statement () (Episodes.getEpisodesByUser targetUserId 10 0)
    pure (shows', episodes)

  case additionalData of
    Left _err -> do
      Log.logInfo "Failed to fetch user activity from database" ()
      pure ([], [])
    Right result -> pure result
