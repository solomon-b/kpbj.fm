{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Edit.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Edit.Get.Templates.Page (template)
import API.Links (apiLinks)
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
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Lucid qualified

--------------------------------------------------------------------------------

-- | All data needed to render the user edit page.
data UserEditViewData = UserEditViewData
  { uevUserMetadata :: UserMetadata.Model,
    uevSidebarShows :: [Shows.Model],
    uevSelectedShow :: Maybe Shows.Model,
    uevBackend :: StorageBackend,
    uevTargetUser :: User.Model,
    uevTargetMetadata :: UserMetadata.Model
  }

-- | Business logic: fetch target user.
action ::
  User.Model ->
  UserMetadata.Model ->
  User.Id ->
  ExceptT HandlerError AppM UserEditViewData
action user userMetadata targetUserId = do
  -- 1. Get storage backend
  backend <- asks getter

  -- 2. Fetch shows for sidebar
  sidebarShows <- lift $ fetchShowsForUser user userMetadata
  let selectedShow = listToMaybe sidebarShows

  -- 3. Fetch target user and their metadata
  (targetUser, targetMetadata) <- fetchTargetUserOrNotFound targetUserId

  pure
    UserEditViewData
      { uevUserMetadata = userMetadata,
        uevSidebarShows = sidebarShows,
        uevSelectedShow = selectedShow,
        uevBackend = backend,
        uevTargetUser = targetUser,
        uevTargetMetadata = targetMetadata
      }

handler ::
  User.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler targetUserId cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "User edit" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata targetUserId
    lift $ renderDashboardTemplate hxRequest vd.uevUserMetadata vd.uevSidebarShows vd.uevSelectedShow NavUsers Nothing Nothing (template vd.uevBackend vd.uevTargetUser vd.uevTargetMetadata)

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
