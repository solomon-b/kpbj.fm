{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.New.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.New.Get.Templates.Page (newBlogPostForm)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotAuthorized)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage)
import Control.Monad (guard, unless)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execTransaction)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Slug ->
  Satisfies 'UserMetadata.Host RoleAuth ->
  Maybe FlashMessage ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug (Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors "New blog post form" apiLinks.rootGet $ do
    vd <- action auth.raUser auth.raMetadata showSlug
    lift $ renderDashboardTemplate hxRequest mFlash vd.bnvUserMetadata (ForShow vd.bnvShowModel) NavBlog Nothing Nothing $ newBlogPostForm vd.bnvShowModel

--------------------------------------------------------------------------------

-- | All data needed to render the new blog post form.
data BlogNewViewData = BlogNewViewData
  { bnvUserMetadata :: UserMetadata.Model,
    bnvShowModel :: Shows.Model
  }

-- | Business logic: show fetch and permission check.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  ExceptT HandlerError AppM BlogNewViewData
action user userMetadata showSlug = do
  -- 1. Fetch show and verify host permissions in a transaction
  showModel <- fetchShowBySlug user userMetadata showSlug
  Log.logInfo "Authorized user accessing new blog post form" showModel.id

  pure
    BlogNewViewData
      { bnvUserMetadata = userMetadata,
        bnvShowModel = showModel
      }

fetchShowBySlug ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  ExceptT HandlerError AppM Shows.Model
fetchShowBySlug user userMetadata showSlug = do
  mResult <-
    fromRightM throwDatabaseError $
      execTransaction $
        runMaybeT $ do
          showModel <- MaybeT $ HT.statement () (Shows.getShowBySlug showSlug)
          -- Admins can create blog posts for any show, hosts need explicit assignment
          unless (UserMetadata.isAdmin userMetadata.mUserRole) $ do
            isHost <- lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) showModel.id)
            guard isHost
          pure showModel

  case mResult of
    Nothing -> throwNotAuthorized "You are not authorized to create blog posts for this show." (Just userMetadata.mUserRole)
    Just sm -> pure sm
