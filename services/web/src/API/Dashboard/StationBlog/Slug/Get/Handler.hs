{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Slug.Get.Handler (handler, action, StationBlogDetailViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Slug.Get.Templates.Page (template)
import API.Links (dashboardStationBlogLinks, rootLink)
import API.Types (DashboardStationBlogRoutes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Maybe (listToMaybe)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as Txn
import Lucid qualified
import Lucid.HTMX
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the station blog post detail page.
data StationBlogDetailViewData = StationBlogDetailViewData
  { sbdvUserMetadata :: UserMetadata.Model,
    sbdvAllShows :: [Shows.Model],
    sbdvSelectedShow :: Maybe Shows.Model,
    sbdvPost :: BlogPosts.Model,
    sbdvTags :: [BlogTags.Model],
    sbdvAuthor :: Maybe UserMetadata.Model
  }

-- | Business logic: fetch post and sidebar data.
action ::
  User.Model ->
  UserMetadata.Model ->
  BlogPosts.Id ->
  ExceptT HandlerError AppM StationBlogDetailViewData
action user userMetadata postId = do
  -- 1. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
      selectedShow = listToMaybe allShows

  -- 2. Fetch blog post with tags and author
  (post, tags, mAuthor) <- fetchBlogPostOrNotFound postId

  pure
    StationBlogDetailViewData
      { sbdvUserMetadata = userMetadata,
        sbdvAllShows = allShows,
        sbdvSelectedShow = selectedShow,
        sbdvPost = post,
        sbdvTags = tags,
        sbdvAuthor = mAuthor
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  BlogPosts.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler postId _slug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Station blog post detail" (dashboardStationBlogLinks.list Nothing) $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata postId
    let postTemplate = template vd.sbdvPost vd.sbdvTags vd.sbdvAuthor
    lift $
      renderDashboardTemplate
        hxRequest
        vd.sbdvUserMetadata
        vd.sbdvAllShows
        vd.sbdvSelectedShow
        NavStationBlog
        Nothing
        (Just actionButton)
        postTemplate

-- | Action button for creating new blog post
actionButton :: Lucid.Html ()
actionButton =
  let newPostUrl = rootLink dashboardStationBlogLinks.newGet
   in Lucid.a_
        [ Lucid.href_ newPostUrl,
          hxGet_ newPostUrl,
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, Tokens.hoverBg]
        ]
        "New Post"

-- | Fetch blog post with tags and author, throwing on error or not found.
fetchBlogPostOrNotFound ::
  BlogPosts.Id ->
  ExceptT HandlerError AppM (BlogPosts.Model, [BlogTags.Model], Maybe UserMetadata.Model)
fetchBlogPostOrNotFound postId =
  fromMaybeM (throwNotFound "Blog post") $
    fromRightM throwDatabaseError $
      execTransaction (fetchBlogPostData postId)

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
