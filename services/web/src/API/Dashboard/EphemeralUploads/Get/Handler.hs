{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.EphemeralUploads.Get.Handler (handler, action, UploadListViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.EphemeralUploads.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardEphemeralUploadsLinks, rootLink)
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
import Data.Has (getter)
import Data.Maybe (fromMaybe, listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the ephemeral uploads list page.
data UploadListViewData = UploadListViewData
  { ulvUserMetadata :: UserMetadata.Model,
    ulvAllShows :: [Shows.Model],
    ulvSelectedShow :: Maybe Shows.Model,
    ulvBackend :: StorageBackend,
    ulvIsStaffOrAdmin :: Bool,
    ulvEphemeralUploads :: [EphemeralUploads.EphemeralUploadWithCreator],
    ulvPage :: PageNumber,
    ulvHasMore :: Bool
  }

-- | Business logic: pagination setup, show/upload fetching.
action ::
  User.Model ->
  UserMetadata.Model ->
  Maybe PageNumber ->
  ExceptT HandlerError AppM UploadListViewData
action user userMetadata maybePage = do
  -- 1. Get storage backend
  backend <- asks getter

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

  -- 4. Fetch ephemeral uploads (staff/admin see flagged uploads too)
  let isStaffOrAdmin = UserMetadata.isStaffOrHigher userMetadata.mUserRole
  allEphemeralUploads <- fetchEphemeralUploads isStaffOrAdmin limit offset

  -- 5. Paginate results
  let ephemeralUploads = take (fromIntegral limit) allEphemeralUploads
      hasMore = length allEphemeralUploads > fromIntegral limit

  pure
    UploadListViewData
      { ulvUserMetadata = userMetadata,
        ulvAllShows = allShows,
        ulvSelectedShow = selectedShow,
        ulvBackend = backend,
        ulvIsStaffOrAdmin = isStaffOrAdmin,
        ulvEphemeralUploads = ephemeralUploads,
        ulvPage = page,
        ulvHasMore = hasMore
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe PageNumber ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Ephemeral uploads list" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata maybePage
    let PageNumber pageNum = vd.ulvPage
        isAppendRequest = hxRequest == IsHxRequest && pageNum > 1
    if isAppendRequest
      then pure $ renderItemsFragment vd.ulvBackend vd.ulvIsStaffOrAdmin vd.ulvEphemeralUploads vd.ulvPage vd.ulvHasMore
      else do
        let ephemeralUploadsTemplate = template vd.ulvBackend vd.ulvEphemeralUploads vd.ulvPage vd.ulvHasMore vd.ulvUserMetadata
        lift $ renderDashboardTemplate hxRequest vd.ulvUserMetadata vd.ulvAllShows vd.ulvSelectedShow NavEphemeralUploads Nothing (Just actionButton) ephemeralUploadsTemplate

fetchEphemeralUploads :: Bool -> Limit -> Offset -> ExceptT HandlerError AppM [EphemeralUploads.EphemeralUploadWithCreator]
fetchEphemeralUploads includeFlagged limit offset =
  fromRightM throwDatabaseError $
    execQuery (EphemeralUploads.getAllEphemeralUploads includeFlagged (limit + 1) offset)

-- | Action button for creating new ephemeral upload
actionButton :: Lucid.Html ()
actionButton =
  let newEphemeralUploadUrl = rootLink dashboardEphemeralUploadsLinks.newGet
   in Lucid.a_
        [ Lucid.href_ newEphemeralUploadUrl,
          hxGet_ newEphemeralUploadUrl,
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-[var(--theme-bg-inverse)] text-[var(--theme-fg-inverse)] px-4 py-2 text-sm font-bold hover:opacity-80"
        ]
        "Upload Ephemeral"
