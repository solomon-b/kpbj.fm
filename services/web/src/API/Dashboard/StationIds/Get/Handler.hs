{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationIds.Get.Handler (handler, action, StationIdsViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.StationIds.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.StationIds.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardStationIdsLinks, rootLink)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Maybe (fromMaybe, listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe PageNumber ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Station IDs list" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata maybePage
    let isAppendRequest = hxRequest == IsHxRequest && vd.sivPage > PageNumber 1
    if isAppendRequest
      then pure $ renderItemsFragment vd.sivBackend vd.sivStationIds vd.sivPage vd.sivHasMore
      else do
        let stationIdsTemplate = template vd.sivBackend vd.sivStationIds vd.sivPage vd.sivHasMore vd.sivUserMetadata
        lift $ renderDashboardTemplate hxRequest vd.sivUserMetadata vd.sivAllShows vd.sivSelectedShow NavStationIds Nothing (Just actionButton) stationIdsTemplate

--------------------------------------------------------------------------------

-- | All data needed to render the station IDs list page.
data StationIdsViewData = StationIdsViewData
  { sivUserMetadata :: UserMetadata.Model,
    sivAllShows :: [Shows.Model],
    sivSelectedShow :: Maybe Shows.Model,
    sivBackend :: StorageBackend,
    sivStationIds :: [StationIds.StationIdWithCreator],
    sivPage :: PageNumber,
    sivHasMore :: Bool
  }

-- | Business logic: pagination, show sidebar, station ID fetching.
action ::
  User.Model ->
  UserMetadata.Model ->
  Maybe PageNumber ->
  ExceptT HandlerError AppM StationIdsViewData
action user userMetadata maybePage = do
  -- 1. Get storage backend for building media URLs
  backend <- asks (Has.getter @StorageBackend)

  -- 2. Set up pagination
  let page@(PageNumber pageNum) = fromMaybe (PageNumber 1) maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (pageNum - 1) * fromIntegral limit :: Offset

  -- 3. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
      selectedShow = listToMaybe allShows

  -- 4. Fetch station IDs (limit + 1 to detect more)
  allStationIds <- fetchStationIds limit offset

  let stationIds = take (fromIntegral limit) allStationIds
      hasMore = length allStationIds > fromIntegral limit

  pure
    StationIdsViewData
      { sivUserMetadata = userMetadata,
        sivAllShows = allShows,
        sivSelectedShow = selectedShow,
        sivBackend = backend,
        sivStationIds = stationIds,
        sivPage = page,
        sivHasMore = hasMore
      }

fetchStationIds :: Limit -> Offset -> ExceptT HandlerError AppM [StationIds.StationIdWithCreator]
fetchStationIds limit offset =
  fromRightM throwDatabaseError $
    execQuery (StationIds.getAllStationIds (limit + 1) offset)

-- | Action button for creating new station ID
actionButton :: Lucid.Html ()
actionButton =
  let newStationIdUrl = rootLink dashboardStationIdsLinks.newGet
   in Lucid.a_
        [ Lucid.href_ newStationIdUrl,
          hxGet_ newStationIdUrl,
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-[var(--theme-bg-inverse)] text-[var(--theme-fg-inverse)] px-4 py-2 text-sm font-bold hover:opacity-80"
        ]
        "Upload Station ID"
