{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.Slug.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardBlogsLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie)
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

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
  Slug ->
  ShowBlogPosts.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug postId cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Show blog post detail" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Fetch the show
    showModel <- fetchShowOrNotFound showSlug

    -- 3. Verify user has access to this show
    requireShowAccess user userMetadata showModel

    -- 4. Fetch the blog post and verify it belongs to this show
    blogPost <- fetchBlogPostOrNotFound postId showModel

    -- 5. Fetch tags for the blog post
    tags <- fromRight [] <$> execQuerySpan (ShowBlogPosts.getTagsForShowBlogPost blogPost.id)

    -- 6. Get user's shows for sidebar
    userShows <- fetchShowsForUser user userMetadata

    -- 7. Render template
    let content = template userMetadata showModel blogPost tags
    renderDashboardTemplate hxRequest userMetadata userShows (Just showModel) NavBlog Nothing (actionButton showModel) content

-- | Action button for creating a new blog post
actionButton :: Shows.Model -> Maybe (Lucid.Html ())
actionButton showModel =
  let newBlogUrl = Links.linkURI $ dashboardBlogsLinks.newGet showModel.slug
   in Just $
        Lucid.a_
          [ Lucid.href_ [i|/#{newBlogUrl}|],
            hxGet_ [i|/#{newBlogUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
          ]
          "New Post"

-- | Fetch show by slug, throwing NotFound if not found
fetchShowOrNotFound ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    Has Tracer env
  ) =>
  Slug ->
  m Shows.Model
fetchShowOrNotFound slug =
  execQuerySpan (Shows.getShowBySlug slug) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

-- | Verify user has access to the show (admin or assigned host)
requireShowAccess ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    Has Tracer env
  ) =>
  User.Model ->
  UserMetadata.Model ->
  Shows.Model ->
  m ()
requireShowAccess user userMetadata showModel =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then pure ()
    else do
      userShows <- fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))
      if any (\s -> s.id == showModel.id) userShows
        then pure ()
        else throwNotAuthorized "You don't have access to this show."

-- | Fetch blog post by ID and verify it belongs to the show
fetchBlogPostOrNotFound ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    Has Tracer env
  ) =>
  ShowBlogPosts.Id ->
  Shows.Model ->
  m ShowBlogPosts.Model
fetchBlogPostOrNotFound postId showModel =
  execQuerySpan (ShowBlogPosts.getShowBlogPostById postId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Blog post"
    Right (Just blogPost)
      | blogPost.showId /= showModel.id -> throwNotFound "Blog post"
      | otherwise -> pure blogPost

-- | Fetch shows for user based on role
fetchShowsForUser ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    Has Tracer env
  ) =>
  User.Model ->
  UserMetadata.Model ->
  m [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuerySpan Shows.getAllActiveShows
    else fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))
