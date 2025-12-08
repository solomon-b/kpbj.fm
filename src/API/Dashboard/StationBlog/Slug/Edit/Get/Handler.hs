{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Slug.Edit.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Slug.Edit.Get.Templates.Form (template)
import API.Links (apiLinks, dashboardStationBlogLinks, userLinks)
import API.Types (DashboardStationBlogRoutes (..), Routes (..), UserRoutes (..))
import App.Common (getUserInfo, renderDashboardTemplate)
import Component.Banner (BannerType (..))
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (BannerParams (..), redirectTemplate, redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Maybe (listToMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug, matchSlug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

dashboardStationBlogGetUrl :: Links.URI
dashboardStationBlogGetUrl = Links.linkURI $ dashboardStationBlogLinks.list Nothing

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
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer blogPostId urlSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to blog edit" ()
      let banner = BannerParams Error "Login Required" "You must be logged in to edit blog posts."
      pure $ Servant.noHeader (redirectWithBanner [i|/#{userLoginGetUrl}|] banner)
    Just (_user, userMetadata)
      | not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) || isSuspended userMetadata -> do
          Log.logInfo "Non-staff user tried to access station blog edit" ()
          let banner = BannerParams Error "Staff Access Required" "You do not have permission to edit station blog posts."
          pure $ Servant.noHeader (redirectWithBanner [i|/#{rootGetUrl}|] banner)
    Just (user, userMetadata) -> do
      -- Fetch shows for sidebar
      showsResult <-
        if UserMetadata.isAdmin userMetadata.mUserRole
          then execQuerySpan Shows.getAllActiveShows
          else execQuerySpan (Shows.getShowsForUser (User.mId user))
      let allShows = fromRight [] showsResult
          selectedShow = listToMaybe allShows

      mResult <- execTransactionSpan $ runMaybeT $ do
        blogPost <- MaybeT $ HT.statement () (BlogPosts.getBlogPostById blogPostId)
        tags <- lift $ HT.statement () (BlogPosts.getTagsForPost blogPost.bpmId)
        MaybeT $ pure $ Just (blogPost, tags)

      case mResult of
        Left err -> do
          Log.logAttention "getBlogPostById execution error" (show err)
          let banner = BannerParams Warning "Blog Post Not Found" "The blog post you're trying to edit doesn't exist."
          html <- renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavStationBlog Nothing Nothing (redirectWithBanner [i|/#{dashboardStationBlogGetUrl}|] banner)
          pure $ Servant.noHeader html
        Right Nothing -> do
          Log.logInfo "No blog post found with id" blogPostId
          let banner = BannerParams Warning "Blog Post Not Found" "The blog post you're trying to edit doesn't exist."
          html <- renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavStationBlog Nothing Nothing (redirectWithBanner [i|/#{dashboardStationBlogGetUrl}|] banner)
          pure $ Servant.noHeader html
        Right (Just (blogPost, tags)) -> do
          let canonicalSlug = blogPost.bpmSlug
              postIdText = display blogPostId
              slugText = display canonicalSlug
              canonicalUrl = [i|/dashboard/station-blog/#{postIdText}/#{slugText}/edit|]

          if matchSlug canonicalSlug (Just urlSlug)
            then do
              Log.logInfo "Authorized user accessing blog edit form" blogPost.bpmId
              let editTemplate = template blogPost tags userMetadata
              html <- renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavStationBlog Nothing Nothing editTemplate
              pure $ Servant.noHeader html
            else do
              Log.logInfo "Redirecting to canonical blog edit URL" canonicalUrl
              html <- renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavStationBlog Nothing Nothing (redirectTemplate canonicalUrl)
              pure $ Servant.addHeader canonicalUrl html
