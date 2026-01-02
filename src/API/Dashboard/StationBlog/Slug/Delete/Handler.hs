{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.StationBlog.Slug.Delete.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (dashboardStationBlogLinks, rootLink)
import API.Types (DashboardStationBlogRoutes (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  m (Lucid.Html ())
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
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m,
    MonadThrow m
  ) =>
  BlogPosts.Id ->
  m BlogPosts.Model
fetchBlogPost postId =
  execQuerySpan (BlogPosts.getBlogPostById postId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Blog post"
    Right (Just post) -> pure post

execDeleteBlogPost ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m,
    MonadThrow m
  ) =>
  BlogPosts.Model ->
  m ()
execDeleteBlogPost blogPost =
  execQuerySpan (BlogPosts.deleteBlogPost blogPost.bpmId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Blog post"
    Right (Just _) -> pure ()
