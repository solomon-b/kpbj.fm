{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.New.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.New.Get.Templates.Page (newBlogPostForm)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotAuthorized)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad (guard, unless)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Either (fromRight)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery, execTransaction)
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
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "New blog post form" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to create blog posts." userMetadata
    vd <- action user userMetadata showSlug
    lift $ renderDashboardTemplate hxRequest vd.bnvUserMetadata vd.bnvAllShows (Just vd.bnvShowModel) NavBlog Nothing Nothing $ newBlogPostForm vd.bnvShowModel

--------------------------------------------------------------------------------

-- | All data needed to render the new blog post form.
data BlogNewViewData = BlogNewViewData
  { bnvUserMetadata :: UserMetadata.Model,
    bnvAllShows :: [Shows.Model],
    bnvShowModel :: Shows.Model
  }

-- | Business logic: show fetch and permission check, sidebar shows.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  ExceptT HandlerError AppM BlogNewViewData
action user userMetadata showSlug = do
  -- 1. Fetch show and verify host permissions in a transaction
  showModel <- fetchShowBySlug user userMetadata showSlug

  -- 2. Fetch shows for dashboard sidebar
  Log.logInfo "Authorized user accessing new blog post form" showModel.id
  allShows <-
    lift $
      if UserMetadata.isAdmin userMetadata.mUserRole
        then fromRight [] <$> execQuery Shows.getAllActiveShows
        else fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))

  pure
    BlogNewViewData
      { bnvUserMetadata = userMetadata,
        bnvAllShows = allShows,
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
