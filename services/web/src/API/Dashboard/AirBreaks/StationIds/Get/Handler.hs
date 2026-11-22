{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.AirBreaks.StationIds.Get.Handler (handler, action, StationIdsViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.StationIds.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.AirBreaks.StationIds.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardStationIdsLinks, rootLink)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.Button ((&))
import Component.Button qualified as Button
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Data.Maybe (fromMaybe)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe PageNumber ->
  Satisfies 'UserMetadata.Host RoleAuth ->
  Maybe FlashMessage ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage (Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors "Station IDs list" apiLinks.rootGet $ do
    vd <- action auth.raMetadata maybePage
    let isAppendRequest = hxRequest == IsHxRequest && vd.sivPage > PageNumber 1
    if isAppendRequest
      then pure $ renderItemsFragment vd.sivBackend vd.sivStationIds vd.sivPage vd.sivHasMore
      else do
        let stationIdsTemplate = template vd.sivBackend vd.sivStationIds vd.sivPage vd.sivHasMore vd.sivUserMetadata
        lift $ renderDashboardTemplate hxRequest mFlash vd.sivUserMetadata NoShow NavStationIds Nothing (Just actionButton) stationIdsTemplate

--------------------------------------------------------------------------------

-- | All data needed to render the station IDs list page.
data StationIdsViewData = StationIdsViewData
  { sivUserMetadata :: UserMetadata.Model,
    sivBackend :: StorageBackend,
    sivStationIds :: [StationIds.StationIdWithCreator],
    sivPage :: PageNumber,
    sivHasMore :: Bool
  }

-- | Business logic: pagination and station ID fetching.
action ::
  UserMetadata.Model ->
  Maybe PageNumber ->
  ExceptT HandlerError AppM StationIdsViewData
action userMetadata maybePage = do
  -- 1. Get storage backend for building media URLs
  backend <- asks (Has.getter @StorageBackend)

  -- 2. Set up pagination
  let page@(PageNumber pageNum) = fromMaybe (PageNumber 1) maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (pageNum - 1) * fromIntegral limit :: Offset

  -- 3. Fetch station IDs (limit + 1 to detect more)
  allStationIds <- fetchStationIds limit offset

  let stationIds = take (fromIntegral limit) allStationIds
      hasMore = length allStationIds > fromIntegral limit

  pure
    StationIdsViewData
      { sivUserMetadata = userMetadata,
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
   in Button.render (Button.defaults & Button.primary & Button.dashboardLink newStationIdUrl) "Upload Station ID"
