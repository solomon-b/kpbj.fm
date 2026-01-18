{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.StationBlog.Slug.Delete.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (dashboardStationBlogLinks, rootLink)
import API.Types (DashboardStationBlogRoutes (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler _tracer postId _postSlug cookie =
  handleBannerErrors "Blog delete" $ do
    -- 1. Require authentication
    (_user, userMetadata) <- requireAuth cookie

    -- 2. Check authorization (must be staff+ and not suspended)
    requireStaffNotSuspended "You don't have permission to delete blog posts." userMetadata

    -- 3. Fetch blog post
    blogPost <- fetchBlogPost postId

    -- 4. Delete it
    execDeleteBlogPost blogPost

    -- 5. Success - redirect with banner
    Log.logInfo "Blog post deleted successfully" postId
    let banner = BannerParams Success "Blog Post Deleted" "The blog post has been deleted."
        listUrl = rootLink $ dashboardStationBlogLinks.list Nothing
    pure $ redirectWithBanner listUrl banner

--------------------------------------------------------------------------------
-- Inline Helpers

fetchBlogPost ::
  BlogPosts.Id ->
  AppM BlogPosts.Model
fetchBlogPost postId =
  execQuerySpan (BlogPosts.getBlogPostById postId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Blog post"
    Right (Just post) -> pure post

execDeleteBlogPost ::
  BlogPosts.Model ->
  AppM ()
execDeleteBlogPost blogPost =
  execQuerySpan (BlogPosts.deleteBlogPost blogPost.bpmId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Blog post"
    Right (Just _) -> pure ()
