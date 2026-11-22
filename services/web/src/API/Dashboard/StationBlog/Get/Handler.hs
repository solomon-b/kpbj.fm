{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Get.Handler (handler, action, StationBlogListViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.StationBlog.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardStationBlogLinks, rootLink)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.Button ((&))
import Component.Button qualified as Button
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the station blog list page.
data StationBlogListViewData = StationBlogListViewData
  { sbvdUserMetadata :: UserMetadata.Model,
    sbvdPosts :: [BlogPosts.Model],
    sbvdPage :: Int64,
    sbvdHasMore :: Bool,
    sbvdIsAppendRequest :: Bool
  }

-- | Business logic: pagination and fetch blog posts.
action ::
  UserMetadata.Model ->
  Maybe Int64 ->
  HxRequest ->
  ExceptT HandlerError AppM StationBlogListViewData
action userMetadata maybePage hxRequest = do
  -- 1. Set up pagination
  let page = fromMaybe 1 maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
      isAppendRequest = hxRequest == IsHxRequest && page > 1

  -- 2. Fetch blog posts
  allPosts <- fetchBlogPosts limit offset

  -- 3. Build view data
  let posts = take (fromIntegral limit) allPosts
      hasMore = length allPosts > fromIntegral limit

  pure
    StationBlogListViewData
      { sbvdUserMetadata = userMetadata,
        sbvdPosts = posts,
        sbvdPage = page,
        sbvdHasMore = hasMore,
        sbvdIsAppendRequest = isAppendRequest
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe Int64 ->
  Satisfies 'UserMetadata.Staff RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Station blog list" apiLinks.rootGet $ do
    vd <- action auth.raMetadata maybePage hxRequest
    if vd.sbvdIsAppendRequest
      then pure $ renderItemsFragment vd.sbvdPosts vd.sbvdPage vd.sbvdHasMore
      else do
        let postsTemplate = template vd.sbvdPosts vd.sbvdPage vd.sbvdHasMore
        lift $
          renderDashboardTemplate
            hxRequest
            vd.sbvdUserMetadata
            NoShow
            NavStationBlog
            Nothing
            (Just actionButton)
            postsTemplate

-- | Action button for creating new blog post
actionButton :: Lucid.Html ()
actionButton =
  let newPostUrl = rootLink dashboardStationBlogLinks.newGet
   in Button.render (Button.defaults & Button.primary & Button.dashboardLink newPostUrl) "New Post"

fetchBlogPosts ::
  Limit ->
  Offset ->
  ExceptT HandlerError AppM [BlogPosts.Model]
fetchBlogPosts limit offset =
  fromRightM throwDatabaseError $
    execQuery (BlogPosts.getAllBlogPosts (limit + 1) offset)
