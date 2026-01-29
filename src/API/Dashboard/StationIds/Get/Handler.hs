{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationIds.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StationIds.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.StationIds.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardStationIdsLinks, rootLink)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Maybe (fromMaybe, listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Maybe PageNumber ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Station IDs list" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Get storage backend for building media URLs
    backend <- asks (Has.getter @StorageBackend)

    -- 3. Set up pagination
    let page@(PageNumber pageNum) = fromMaybe (PageNumber 1) maybePage
        limit = 20 :: Limit
        offset = fromIntegral $ (pageNum - 1) * fromIntegral limit :: Offset
        isAppendRequest = hxRequest == IsHxRequest && pageNum > 1

    -- 4. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuerySpan Shows.getAllActiveShows
        else execQuerySpan (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    -- 5. Fetch station IDs
    allStationIds <- fetchStationIds limit offset

    -- 6. Render response
    let stationIds = take (fromIntegral limit) allStationIds
        hasMore = length allStationIds > fromIntegral limit

    if isAppendRequest
      then pure $ renderItemsFragment backend stationIds page hasMore
      else do
        let stationIdsTemplate = template backend stationIds page hasMore userMetadata
        renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavStationIds Nothing (Just actionButton) stationIdsTemplate

fetchStationIds :: Limit -> Offset -> AppM [StationIds.StationIdWithCreator]
fetchStationIds limit offset = do
  stationIdsResult <- execQuerySpan (StationIds.getAllStationIds (limit + 1) offset)
  case stationIdsResult of
    Left err -> throwDatabaseError err
    Right stationIds -> pure stationIds

-- | Action button for creating new station ID
actionButton :: Lucid.Html ()
actionButton =
  let newStationIdUrl = rootLink dashboardStationIdsLinks.newGet
   in Lucid.a_
        [ Lucid.href_ newStationIdUrl,
          hxGet_ newStationIdUrl,
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
        ]
        "Upload Station ID"
