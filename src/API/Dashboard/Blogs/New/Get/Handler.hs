{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.New.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.New.Get.Templates.Page (newBlogPostForm)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotAuthorized)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad (guard, unless)
import Control.Monad.Trans (lift)
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

--------------------------------------------------------------------------------

handler ::
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "New blog post form" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to create blog posts." userMetadata

    -- 2. Fetch show and verify host permissions in a transaction
    showModel <- fetchShowBySlug user userMetadata showSlug

    -- 3. Fetch shows for dashboard sidebar
    Log.logInfo "Authorized user accessing new blog post form" showModel.id
    allShows <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then fromRight [] <$> execQuery Shows.getAllActiveShows
        else fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))

    -- 4. Render form
    renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavBlog Nothing Nothing $ newBlogPostForm showModel

fetchShowBySlug ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  AppM Shows.Model
fetchShowBySlug user userMetadata showSlug = do
  mResult <- execTransaction $ runMaybeT $ do
    showModel <- MaybeT $ HT.statement () (Shows.getShowBySlug showSlug)
    -- Admins can create blog posts for any show, hosts need explicit assignment
    unless (UserMetadata.isAdmin userMetadata.mUserRole) $ do
      isHost <- lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) showModel.id)
      guard isHost
    pure showModel

  case mResult of
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotAuthorized "You are not authorized to create blog posts for this show." (Just userMetadata.mUserRole)
    Right (Just sm) -> pure sm
