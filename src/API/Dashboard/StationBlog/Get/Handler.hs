{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.StationBlog.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardStationBlogLinks, rootLink)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Maybe Int64 ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Station blog list" apiLinks.rootGet $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Set up pagination
    let page = fromMaybe 1 maybePage
        limit = 20 :: Limit
        offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
        isAppendRequest = hxRequest == IsHxRequest && page > 1

    -- 3. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuerySpan Shows.getAllActiveShows
        else execQuerySpan (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    -- 4. Fetch blog posts
    allPosts <- fetchBlogPosts limit offset

    -- 5. Render response
    let posts = take (fromIntegral limit) allPosts
        hasMore = length allPosts > fromIntegral limit

    if isAppendRequest
      then pure $ renderItemsFragment posts page hasMore
      else do
        let postsTemplate = template posts page hasMore
        renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavStationBlog Nothing (Just actionButton) postsTemplate

-- | Action button for creating new blog post
actionButton :: Lucid.Html ()
actionButton =
  let newPostUrl = rootLink dashboardStationBlogLinks.newGet
   in Lucid.a_
        [ Lucid.href_ newPostUrl,
          hxGet_ newPostUrl,
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
        ]
        "New Post"

fetchBlogPosts ::
  Limit ->
  Offset ->
  AppM [BlogPosts.Model]
fetchBlogPosts limit offset =
  execQuerySpan (BlogPosts.getAllBlogPosts (limit + 1) offset) >>= \case
    Left err -> throwDatabaseError err
    Right posts -> pure posts
