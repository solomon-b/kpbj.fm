{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Slug.Edit.Get.Handler (handler, action, StationBlogEditViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Slug.Edit.Get.Templates.Form (template)
import API.Links (dashboardStationBlogLinks)
import API.Types (DashboardStationBlogRoutes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (redirectTemplate)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Either (fromRight)
import Data.Has (getter)
import Data.Maybe (listToMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug, matchSlug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Servant qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the blog post edit form.
data StationBlogEditViewData = StationBlogEditViewData
  { sbevUserMetadata :: UserMetadata.Model,
    sbevAllShows :: [Shows.Model],
    sbevSelectedShow :: Maybe Shows.Model,
    sbevBlogPost :: BlogPosts.Model,
    sbevTags :: [BlogTags.Model],
    sbevStorageBackend :: StorageBackend,
    sbevCanonicalUrl :: Text
  }

-- | Business logic: fetch post and tags, build view data.
action ::
  User.Model ->
  UserMetadata.Model ->
  BlogPosts.Id ->
  ExceptT HandlerError AppM StationBlogEditViewData
action user userMetadata blogPostId = do
  -- 1. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
      selectedShow = listToMaybe allShows

  -- 2. Fetch blog post and tags
  mResult <-
    fromRightM throwDatabaseError $
      execTransaction $
        runMaybeT $ do
          blogPost <- MaybeT $ HT.statement () (BlogPosts.getBlogPostById blogPostId)
          tags <- lift $ HT.statement () (BlogPosts.getTagsForPost blogPost.bpmId)
          pure (blogPost, tags)

  (blogPost, tags) <- case mResult of
    Nothing -> throwNotFound "Blog post"
    Just result -> pure result

  -- 3. Get storage backend for URL construction
  storageBackend <- asks getter

  -- 4. Build canonical URL
  let postIdText = display blogPostId
      slugText = display blogPost.bpmSlug
      canonicalUrl = [i|/dashboard/station-blog/#{postIdText}/#{slugText}/edit|]

  pure
    StationBlogEditViewData
      { sbevUserMetadata = userMetadata,
        sbevAllShows = allShows,
        sbevSelectedShow = selectedShow,
        sbevBlogPost = blogPost,
        sbevTags = tags,
        sbevStorageBackend = storageBackend,
        sbevCanonicalUrl = canonicalUrl
      }

-- | Servant handler: thin glue composing action + render, with canonical slug redirect.
handler ::
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler blogPostId urlSlug cookie (foldHxReq -> hxRequest) =
  handleRedirectErrors "Station blog edit form" (dashboardStationBlogLinks.list Nothing) $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to edit station blog posts." userMetadata
    vd <- action user userMetadata blogPostId
    if matchSlug vd.sbevBlogPost.bpmSlug (Just urlSlug)
      then do
        Log.logInfo "Authorized user accessing blog edit form" vd.sbevBlogPost.bpmId
        let editTemplate = template vd.sbevStorageBackend vd.sbevBlogPost vd.sbevTags vd.sbevUserMetadata
        html <-
          lift $
            renderDashboardTemplate
              hxRequest
              vd.sbevUserMetadata
              vd.sbevAllShows
              vd.sbevSelectedShow
              NavStationBlog
              Nothing
              Nothing
              editTemplate
        pure $ Servant.noHeader html
      else do
        Log.logInfo "Redirecting to canonical blog edit URL" vd.sbevCanonicalUrl
        html <-
          lift $
            renderDashboardTemplate
              hxRequest
              vd.sbevUserMetadata
              vd.sbevAllShows
              vd.sbevSelectedShow
              NavStationBlog
              Nothing
              Nothing
              (redirectTemplate vd.sbevCanonicalUrl)
        pure $ Servant.addHeader vd.sbevCanonicalUrl html
