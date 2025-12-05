{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Slug.Get (Route, handler) where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (dashboardStationBlogGetLink, dashboardStationBlogNewGetLink, rootGetLink, userLoginGetLink)
import API.Dashboard.StationBlog.Slug.Get.Templates.Page (template)
import App.Common (getUserInfo, renderDashboardTemplate)
import Component.Banner (BannerType (..))
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Maybe (listToMaybe)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as Txn
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI rootGetLink

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

dashboardStationBlogNewGetUrl :: Links.URI
dashboardStationBlogNewGetUrl = Links.linkURI dashboardStationBlogNewGetLink

dashboardStationBlogGetUrl :: Links.URI
dashboardStationBlogGetUrl = Links.linkURI dashboardStationBlogGetLink

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard/station-blog/:id/:slug"
    ( "dashboard"
        :> "station-blog"
        :> Servant.Capture "id" BlogPosts.Id
        :> Servant.Capture "slug" Slug
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

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
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer postId _slug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      let banner = BannerParams Error "Login Required" "You must be logged in to access this page."
      pure $ redirectWithBanner [i|/#{userLoginGetUrl}|] banner
    Just (_user, userMetadata)
      | not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) || isSuspended userMetadata -> do
          let banner = BannerParams Error "Staff Access Required" "You do not have permission to access this page."
          pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
    Just (user, userMetadata) -> do
      -- Fetch shows for sidebar (admins see all, staff see their assigned shows)
      showsResult <-
        if UserMetadata.isAdmin userMetadata.mUserRole
          then execQuerySpan Shows.getAllActiveShows
          else execQuerySpan (Shows.getShowsForUser (User.mId user))
      let allShows = either (const []) id showsResult
          selectedShow = listToMaybe allShows

      -- Fetch blog post with tags and author
      execTransactionSpan (fetchBlogPostData postId) >>= \case
        Left _err -> do
          Log.logInfo "Failed to fetch blog post from database" ()
          let banner = BannerParams Error "Error" "Failed to load blog post. Please try again."
          pure $ redirectWithBanner [i|/#{dashboardStationBlogGetUrl}|] banner
        Right Nothing -> do
          Log.logInfo "Blog post not found" postId
          let banner = BannerParams Error "Not Found" "Blog post not found."
          pure $ redirectWithBanner [i|/#{dashboardStationBlogGetUrl}|] banner
        Right (Just (post, tags, mAuthor)) -> do
          let postTemplate = template post tags mAuthor
          renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavStationBlog Nothing (Just actionButton) postTemplate

-- | Action button for creating new blog post
actionButton :: Lucid.Html ()
actionButton =
  Lucid.a_
    [ Lucid.href_ [i|/#{dashboardStationBlogNewGetUrl}|],
      hxGet_ [i|/#{dashboardStationBlogNewGetUrl}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
    ]
    "New Post"

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
