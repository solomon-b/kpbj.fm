{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Slug.Edit.Get.Handler (handler, action, StationBlogEditViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Slug.Edit.Get.Templates.Form (template)
import API.Links (dashboardStationBlogLinks)
import API.Types (DashboardStationBlogRoutes (..))
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (throwHxRedirect)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Has (getter)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug, matchSlug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execTransaction)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the blog post edit form.
data StationBlogEditViewData = StationBlogEditViewData
  { sbevUserMetadata :: UserMetadata.Model,
    sbevBlogPost :: BlogPosts.Model,
    sbevTags :: [BlogTags.Model],
    sbevStorageBackend :: StorageBackend,
    sbevCanonicalUrl :: Text
  }

-- | Business logic: fetch post and tags, build view data.
action ::
  UserMetadata.Model ->
  BlogPosts.Id ->
  ExceptT HandlerError AppM StationBlogEditViewData
action userMetadata blogPostId = do
  -- 1. Fetch blog post and tags
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

  -- 2. Get storage backend for URL construction
  storageBackend <- asks getter

  -- 3. Build canonical URL
  let postIdText = display blogPostId
      slugText = display blogPost.bpmSlug
      canonicalUrl = [i|/dashboard/station-blog/#{postIdText}/#{slugText}/edit|]

  pure
    StationBlogEditViewData
      { sbevUserMetadata = userMetadata,
        sbevBlogPost = blogPost,
        sbevTags = tags,
        sbevStorageBackend = storageBackend,
        sbevCanonicalUrl = canonicalUrl
      }

-- | Servant handler: thin glue composing action + render, with canonical slug redirect.
handler ::
  BlogPosts.Id ->
  Slug ->
  Satisfies 'UserMetadata.Staff RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler blogPostId urlSlug (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleRedirectErrors "Station blog edit form" (dashboardStationBlogLinks.list Nothing) $ do
    vd <- action auth.raMetadata blogPostId
    if matchSlug vd.sbevBlogPost.bpmSlug (Just urlSlug)
      then do
        Log.logInfo "Authorized user accessing blog edit form" vd.sbevBlogPost.bpmId
        let editTemplate = template vd.sbevStorageBackend vd.sbevBlogPost vd.sbevTags vd.sbevUserMetadata
        lift $
          renderDashboardTemplate
            hxRequest
            vd.sbevUserMetadata
            NoShow
            NavStationBlog
            Nothing
            Nothing
            editTemplate
      else do
        Log.logInfo "Redirecting to canonical blog edit URL" vd.sbevCanonicalUrl
        lift $ throwHxRedirect vd.sbevCanonicalUrl Nothing
