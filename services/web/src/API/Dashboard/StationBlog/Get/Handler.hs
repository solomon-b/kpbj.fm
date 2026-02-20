{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Get.Handler (handler, action, StationBlogListViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.StationBlog.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardStationBlogLinks, rootLink)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the station blog list page.
data StationBlogListViewData = StationBlogListViewData
  { sbvdUserMetadata :: UserMetadata.Model,
    sbvdAllShows :: [Shows.Model],
    sbvdSelectedShow :: Maybe Shows.Model,
    sbvdPosts :: [BlogPosts.Model],
    sbvdPage :: Int64,
    sbvdHasMore :: Bool,
    sbvdIsAppendRequest :: Bool
  }

-- | Business logic: pagination, fetch shows and blog posts.
action ::
  User.Model ->
  UserMetadata.Model ->
  Maybe Int64 ->
  HxRequest ->
  ExceptT HandlerError AppM StationBlogListViewData
action user userMetadata maybePage hxRequest = do
  -- 1. Set up pagination
  let page = fromMaybe 1 maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
      isAppendRequest = hxRequest == IsHxRequest && page > 1

  -- 2. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
      selectedShow = listToMaybe allShows

  -- 3. Fetch blog posts
  allPosts <- fetchBlogPosts limit offset

  -- 4. Build view data
  let posts = take (fromIntegral limit) allPosts
      hasMore = length allPosts > fromIntegral limit

  pure
    StationBlogListViewData
      { sbvdUserMetadata = userMetadata,
        sbvdAllShows = allShows,
        sbvdSelectedShow = selectedShow,
        sbvdPosts = posts,
        sbvdPage = page,
        sbvdHasMore = hasMore,
        sbvdIsAppendRequest = isAppendRequest
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe Int64 ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Station blog list" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata maybePage hxRequest
    if vd.sbvdIsAppendRequest
      then pure $ renderItemsFragment vd.sbvdPosts vd.sbvdPage vd.sbvdHasMore
      else do
        let postsTemplate = template vd.sbvdPosts vd.sbvdPage vd.sbvdHasMore
        lift $
          renderDashboardTemplate
            hxRequest
            vd.sbvdUserMetadata
            vd.sbvdAllShows
            vd.sbvdSelectedShow
            NavStationBlog
            Nothing
            (Just actionButton)
            postsTemplate

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

fetchBlogPosts ::
  Limit ->
  Offset ->
  ExceptT HandlerError AppM [BlogPosts.Model]
fetchBlogPosts limit offset =
  fromRightM throwDatabaseError $
    execQuery (BlogPosts.getAllBlogPosts (limit + 1) offset)
