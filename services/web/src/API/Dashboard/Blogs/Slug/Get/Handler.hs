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
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Slug ->
  ShowBlogPosts.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug postId cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Show blog post detail" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata showSlug postId
    let content = template vd.bdvUserMetadata vd.bdvShowModel vd.bdvBlogPost vd.bdvTags
    lift $ renderDashboardTemplate hxRequest vd.bdvUserMetadata vd.bdvAllShows (Just vd.bdvShowModel) NavBlog Nothing (actionButton vd.bdvShowModel) content
  where
    actionButton :: Shows.Model -> Maybe (Lucid.Html ())
    actionButton showModel =
      let newBlogUrl = Links.linkURI $ dashboardBlogsLinks.newGet showModel.slug
       in Just $
            Lucid.a_
              [ Lucid.href_ [i|/#{newBlogUrl}|],
                hxGet_ [i|/#{newBlogUrl}|],
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, Tokens.hoverBg]
              ]
              "New Post"

--------------------------------------------------------------------------------

-- | All data needed to render the blog post detail page.
data BlogDetailViewData = BlogDetailViewData
  { bdvUserMetadata :: UserMetadata.Model,
    bdvAllShows :: [Shows.Model],
    bdvShowModel :: Shows.Model,
    bdvBlogPost :: ShowBlogPosts.Model,
    bdvTags :: [ShowBlogTags.Model]
  }

-- | Business logic: show fetch, access check, blog post fetch, tags fetch.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  ShowBlogPosts.Id ->
  ExceptT HandlerError AppM BlogDetailViewData
action user userMetadata showSlug postId = do
  -- 1. Fetch the show
  showModel <- fetchShowOrNotFound showSlug

  -- 2. Verify user has access to this show
  requireShowAccess user userMetadata showModel

  -- 3. Fetch the blog post and verify it belongs to this show
  blogPost <- fetchBlogPostOrNotFound postId showModel

  -- 4. Fetch tags for the blog post
  tags <- fromRight [] <$> execQuery (ShowBlogPosts.getTagsForShowBlogPost blogPost.id)

  -- 5. Get user's shows for sidebar
  allShows <- lift $ fetchShowsForUser user userMetadata

  pure
    BlogDetailViewData
      { bdvUserMetadata = userMetadata,
        bdvAllShows = allShows,
        bdvShowModel = showModel,
        bdvBlogPost = blogPost,
        bdvTags = tags
      }

-- | Fetch show by slug, throwing NotFound if not found
fetchShowOrNotFound ::
  Slug ->
  ExceptT HandlerError AppM Shows.Model
fetchShowOrNotFound slug =
  fromMaybeM (throwNotFound "Show") $
    fromRightM throwDatabaseError $
      execQuery (Shows.getShowBySlug slug)

-- | Verify user has access to the show (admin or assigned host)
requireShowAccess ::
  User.Model ->
  UserMetadata.Model ->
  Shows.Model ->
  ExceptT HandlerError AppM ()
requireShowAccess user userMetadata showModel =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then pure ()
    else do
      userShows <- fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))
      if any (\s -> s.id == showModel.id) userShows
        then pure ()
        else throwNotAuthorized "You don't have access to this show." (Just userMetadata.mUserRole)

-- | Fetch blog post by ID and verify it belongs to the show
fetchBlogPostOrNotFound ::
  ShowBlogPosts.Id ->
  Shows.Model ->
  ExceptT HandlerError AppM ShowBlogPosts.Model
fetchBlogPostOrNotFound postId showModel = do
  blogPost <-
    fromMaybeM (throwNotFound "Blog post") $
      fromRightM throwDatabaseError $
        execQuery (ShowBlogPosts.getShowBlogPostById postId)
  if blogPost.showId /= showModel.id
    then throwNotFound "Blog post"
    else pure blogPost

-- | Fetch shows for user based on role
fetchShowsForUser ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuery Shows.getAllActiveShows
    else fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))
