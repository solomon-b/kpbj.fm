{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Slug.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Slug.Get.Templates.Page (template)
import API.Links (dashboardStationBlogLinks, rootLink)
import API.Types (DashboardStationBlogRoutes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotFound)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as Txn
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
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
    Has HSQL.Pool.Pool env,
    Has (Maybe GoogleAnalyticsId) env
  ) =>
  Tracer ->
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer postId _slug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Station blog post detail" (dashboardStationBlogLinks.list Nothing) $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuerySpan Shows.getAllActiveShows
        else execQuerySpan (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    -- 3. Fetch blog post with tags and author
    (post, tags, mAuthor) <- fetchBlogPostOrNotFound postId

    -- 4. Render template
    let postTemplate = template post tags mAuthor
    renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavStationBlog Nothing (Just actionButton) postTemplate

-- | Action button for creating new blog post
actionButton :: Lucid.Html ()
actionButton =
  let newPostUrl = rootLink dashboardStationBlogLinks.newGet
   in Lucid.a_
        [ Lucid.href_ newPostUrl,
          hxGet_ newPostUrl,
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
        ]
        "New Post"

-- | Fetch blog post with tags and author, throwing on error or not found.
fetchBlogPostOrNotFound ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    Has Tracer env
  ) =>
  BlogPosts.Id ->
  m (BlogPosts.Model, [BlogTags.Model], Maybe UserMetadata.Model)
fetchBlogPostOrNotFound postId =
  execTransactionSpan (fetchBlogPostData postId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Blog post"
    Right (Just result) -> pure result

-- | Fetch blog post data for detail view
fetchBlogPostData ::
  BlogPosts.Id ->
  Txn.Transaction (Maybe (BlogPosts.Model, [BlogTags.Model], Maybe UserMetadata.Model))
fetchBlogPostData postId = do
  mPost <- Txn.statement () (BlogPosts.getBlogPostById postId)
  case mPost of
    Nothing -> pure Nothing
    Just post -> do
      tags <- Txn.statement () (BlogPosts.getTagsForPost postId)
      mAuthor <- Txn.statement () (UserMetadata.getUserMetadata post.bpmAuthorId)
      pure $ Just (post, tags, mAuthor)
