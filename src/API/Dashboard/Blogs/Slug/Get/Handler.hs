{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.Slug.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import API.Dashboard.Get.Templates.Auth (notAuthorizedTemplate, notLoggedInTemplate)
import API.Links (dashboardBlogsLinks)
import API.Types (DashboardBlogsRoutes (..))
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie)
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
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Slug ->
  ShowBlogPosts.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug postId cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to dashboard blog post" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (_, userMetadata)
      | not (UserMetadata.isHostOrHigher userMetadata.mUserRole) -> do
          Log.logInfo "User without Host role tried to access dashboard blog post" userMetadata.mDisplayName
          renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
    Just (user, userMetadata) -> do
      -- Fetch the show by slug
      execQuerySpan (Shows.getShowBySlug showSlug) >>= \case
        Left _err -> do
          Log.logInfo "Failed to fetch show from database" showSlug
          let content = errorTemplate "Failed to load show. Please try again."
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavBlog Nothing Nothing content
        Right Nothing -> do
          Log.logInfo "Show not found" showSlug
          let content = notFoundTemplate
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavBlog Nothing Nothing content
        Right (Just showModel) -> do
          -- Verify user has access to this show (unless admin)
          hasAccess <-
            if UserMetadata.isAdmin userMetadata.mUserRole
              then pure True
              else do
                execQuerySpan (Shows.getShowsForUser (User.mId user)) >>= \case
                  Left _ -> pure False
                  Right userShows -> pure $ any (\s -> s.id == showModel.id) userShows

          if not hasAccess
            then do
              Log.logInfo "User tried to access blog post for show they don't have access to" (userMetadata.mDisplayName, showSlug)
              renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
            else do
              -- Fetch the blog post
              execQuerySpan (ShowBlogPosts.getShowBlogPostById postId) >>= \case
                Left _err -> do
                  Log.logInfo "Failed to fetch blog post from database" postId
                  let content = errorTemplate "Failed to load blog post. Please try again."
                  renderDashboardTemplate hxRequest userMetadata [showModel] (Just showModel) NavBlog Nothing Nothing content
                Right Nothing -> do
                  Log.logInfo "Blog post not found" postId
                  let content = notFoundTemplate
                  renderDashboardTemplate hxRequest userMetadata [showModel] (Just showModel) NavBlog Nothing Nothing content
                Right (Just blogPost) -> do
                  -- Verify blog post belongs to the show
                  if blogPost.showId /= showModel.id
                    then do
                      Log.logInfo "Blog post does not belong to show" (showSlug, postId)
                      let content = errorTemplate "Blog post not found in this show."
                      renderDashboardTemplate hxRequest userMetadata [showModel] (Just showModel) NavBlog Nothing Nothing content
                    else do
                      -- Fetch tags for the blog post
                      tags <- fromRight [] <$> execQuerySpan (ShowBlogPosts.getTagsForShowBlogPost blogPost.id)

                      -- Get user's shows for sidebar
                      userShows <-
                        if UserMetadata.isAdmin userMetadata.mUserRole
                          then fromRight [] <$> execQuerySpan Shows.getAllActiveShows
                          else fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))

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
