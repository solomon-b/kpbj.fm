{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.StationBlog.Slug.Delete.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Links (dashboardStationBlogLinks, rootLink)
import API.Types (DashboardStationBlogRoutes (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Trans.Except (ExceptT)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Business logic: fetch, delete blog post.
action ::
  BlogPosts.Id ->
  ExceptT HandlerError AppM ()
action postId = do
  -- 1. Fetch blog post
  blogPost <- fetchBlogPost postId

  -- 2. Delete it
  execDeleteBlogPost blogPost

  -- 3. Log success
  Log.logInfo "Blog post deleted successfully" postId

-- | Servant handler: thin glue composing action + return banner redirect.
handler ::
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler postId _postSlug cookie =
  handleBannerErrors "Blog delete" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You don't have permission to delete blog posts." userMetadata
    action postId
    let banner = BannerParams Success "Blog Post Deleted" "The blog post has been deleted."
        listUrl = rootLink $ dashboardStationBlogLinks.list Nothing
    pure $ redirectWithBanner listUrl banner

--------------------------------------------------------------------------------
-- Inline Helpers

fetchBlogPost ::
  BlogPosts.Id ->
  ExceptT HandlerError AppM BlogPosts.Model
fetchBlogPost postId =
  fromMaybeM (throwNotFound "Blog post") $
    fromRightM throwDatabaseError $
      execQuery (BlogPosts.getBlogPostById postId)

execDeleteBlogPost ::
  BlogPosts.Model ->
  ExceptT HandlerError AppM ()
execDeleteBlogPost blogPost = do
  result <- execQuery (BlogPosts.deleteBlogPost blogPost.bpmId)
  case result of
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Blog post"
    Right (Just _) -> pure ()
