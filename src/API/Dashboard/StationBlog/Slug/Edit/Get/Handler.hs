{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Slug.Edit.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Slug.Edit.Get.Templates.Form (template)
import API.Links (dashboardStationBlogLinks)
import API.Types (DashboardStationBlogRoutes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (redirectTemplate)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
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
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer blogPostId urlSlug cookie (foldHxReq -> hxRequest) =
  handleRedirectErrors "Station blog edit form" (dashboardStationBlogLinks.list Nothing) $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to edit station blog posts." userMetadata

    -- 2. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuerySpan Shows.getAllActiveShows
        else execQuerySpan (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    -- 3. Fetch blog post and tags
    mResult <- execTransactionSpan $ runMaybeT $ do
      blogPost <- MaybeT $ HT.statement () (BlogPosts.getBlogPostById blogPostId)
      tags <- lift $ HT.statement () (BlogPosts.getTagsForPost blogPost.bpmId)
      pure (blogPost, tags)

    (blogPost, tags) <- case mResult of
      Left err -> throwDatabaseError err
      Right Nothing -> throwNotFound "Blog post"
      Right (Just result) -> pure result

    -- 4. Get storage backend for URL construction
    storageBackend <- asks getter

    -- 5. Check for canonical URL and render
    let canonicalSlug = blogPost.bpmSlug
        postIdText = display blogPostId
        slugText = display canonicalSlug
        canonicalUrl = [i|/dashboard/station-blog/#{postIdText}/#{slugText}/edit|]

    if matchSlug canonicalSlug (Just urlSlug)
      then do
        Log.logInfo "Authorized user accessing blog edit form" blogPost.bpmId
        let editTemplate = template storageBackend blogPost tags userMetadata
        html <- renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavStationBlog Nothing Nothing editTemplate
        pure $ Servant.noHeader html
      else do
        Log.logInfo "Redirecting to canonical blog edit URL" canonicalUrl
        html <- renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavStationBlog Nothing Nothing (redirectTemplate canonicalUrl)
        pure $ Servant.addHeader canonicalUrl html
