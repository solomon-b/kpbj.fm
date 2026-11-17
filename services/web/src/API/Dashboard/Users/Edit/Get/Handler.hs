{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Edit.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Edit.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has (getter)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execTransaction)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Lucid qualified

--------------------------------------------------------------------------------

-- | All data needed to render the user edit page.
data UserEditViewData = UserEditViewData
  { uevUserMetadata :: UserMetadata.Model,
    uevBackend :: StorageBackend,
    uevTargetUser :: User.Model,
    uevTargetMetadata :: UserMetadata.Model
  }

-- | Business logic: fetch target user.
action ::
  UserMetadata.Model ->
  User.Id ->
  ExceptT HandlerError AppM UserEditViewData
action userMetadata targetUserId = do
  -- 1. Get storage backend
  backend <- asks getter

  -- 2. Fetch target user and their metadata
  (targetUser, targetMetadata) <- fetchTargetUserOrNotFound targetUserId

  pure
    UserEditViewData
      { uevUserMetadata = userMetadata,
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
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action userMetadata targetUserId
    lift $ renderDashboardTemplate hxRequest vd.uevUserMetadata NoShow NavUsers Nothing Nothing (template vd.uevBackend vd.uevTargetUser vd.uevTargetMetadata)

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
