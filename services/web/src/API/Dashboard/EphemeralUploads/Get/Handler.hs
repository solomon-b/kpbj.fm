{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.EphemeralUploads.Get.Handler (handler, action, UploadListViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.EphemeralUploads.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardEphemeralUploadsLinks, rootLink)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.Button ((&))
import Component.Button qualified as Button
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has (getter)
import Data.Maybe (fromMaybe)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the ephemeral uploads list page.
data UploadListViewData = UploadListViewData
  { ulvUserMetadata :: UserMetadata.Model,
    ulvBackend :: StorageBackend,
    ulvIsStaffOrAdmin :: Bool,
    ulvEphemeralUploads :: [EphemeralUploads.EphemeralUploadWithCreator],
    ulvPage :: PageNumber,
    ulvHasMore :: Bool
  }

-- | Business logic: pagination setup and upload fetching.
action ::
  UserMetadata.Model ->
  Maybe PageNumber ->
  ExceptT HandlerError AppM UploadListViewData
action userMetadata maybePage = do
  -- 1. Get storage backend
  backend <- asks getter

  -- 2. Set up pagination
  let page@(PageNumber pageNum) = fromMaybe (PageNumber 1) maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (pageNum - 1) * fromIntegral limit :: Offset

  -- 3. Fetch ephemeral uploads (staff/admin see flagged uploads too)
  let isStaffOrAdmin = UserMetadata.isStaffOrHigher userMetadata.mUserRole
  allEphemeralUploads <- fetchEphemeralUploads isStaffOrAdmin limit offset

  -- 4. Paginate results
  let ephemeralUploads = take (fromIntegral limit) allEphemeralUploads
      hasMore = length allEphemeralUploads > fromIntegral limit

  pure
    UploadListViewData
      { ulvUserMetadata = userMetadata,
        ulvBackend = backend,
        ulvIsStaffOrAdmin = isStaffOrAdmin,
        ulvEphemeralUploads = ephemeralUploads,
        ulvPage = page,
        ulvHasMore = hasMore
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe PageNumber ->
  Satisfies 'UserMetadata.Host RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Ephemeral uploads list" apiLinks.rootGet $ do
    vd <- action auth.raMetadata maybePage
    let PageNumber pageNum = vd.ulvPage
        isAppendRequest = hxRequest == IsHxRequest && pageNum > 1
    if isAppendRequest
      then pure $ renderItemsFragment vd.ulvBackend vd.ulvIsStaffOrAdmin vd.ulvEphemeralUploads vd.ulvPage vd.ulvHasMore
      else do
        let ephemeralUploadsTemplate = template vd.ulvBackend vd.ulvEphemeralUploads vd.ulvPage vd.ulvHasMore vd.ulvUserMetadata
        lift $ renderDashboardTemplate hxRequest vd.ulvUserMetadata NoShow NavEphemeralUploads Nothing (Just actionButton) ephemeralUploadsTemplate

fetchEphemeralUploads :: Bool -> Limit -> Offset -> ExceptT HandlerError AppM [EphemeralUploads.EphemeralUploadWithCreator]
fetchEphemeralUploads includeFlagged limit offset =
  fromRightM throwDatabaseError $
    execQuery (EphemeralUploads.getAllEphemeralUploads includeFlagged (limit + 1) offset)

-- | Action button for creating new ephemeral upload
actionButton :: Lucid.Html ()
actionButton =
  let newEphemeralUploadUrl = rootLink dashboardEphemeralUploadsLinks.newGet
   in Button.render (Button.defaults & Button.primary & Button.dashboardLink newEphemeralUploadUrl) "Upload Ephemeral"
