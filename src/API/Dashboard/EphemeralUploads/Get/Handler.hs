{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.EphemeralUploads.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.EphemeralUploads.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardEphemeralUploadsLinks, rootLink)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Data.Either (fromRight)
import Data.Has (getter)
import Data.Maybe (fromMaybe, listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.PageNumber (PageNumber (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX

--------------------------------------------------------------------------------

handler ::
  Maybe PageNumber ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Ephemeral uploads list" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Get storage backend
    backend <- asks getter

    -- 3. Set up pagination
    let page@(PageNumber pageNum) = fromMaybe (PageNumber 1) maybePage
        limit = 20 :: Limit
        offset = fromIntegral $ (pageNum - 1) * fromIntegral limit :: Offset
        isAppendRequest = hxRequest == IsHxRequest && pageNum > 1

    -- 4. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuery Shows.getAllActiveShows
        else execQuery (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    -- 5. Fetch ephemeral uploads
    allEphemeralUploads <- fetchEphemeralUploads limit offset

    -- 6. Render response
    let ephemeralUploads = take (fromIntegral limit) allEphemeralUploads
        hasMore = length allEphemeralUploads > fromIntegral limit

    if isAppendRequest
      then pure $ renderItemsFragment backend ephemeralUploads page hasMore
      else do
        let ephemeralUploadsTemplate = template backend ephemeralUploads page hasMore userMetadata
        renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavEphemeralUploads Nothing (Just actionButton) ephemeralUploadsTemplate

fetchEphemeralUploads :: Limit -> Offset -> AppM [EphemeralUploads.EphemeralUploadWithCreator]
fetchEphemeralUploads limit offset = do
  ephemeralUploadsResult <- execQuery (EphemeralUploads.getAllEphemeralUploads (limit + 1) offset)
  case ephemeralUploadsResult of
    Left err -> throwDatabaseError err
    Right ephemeralUploads -> pure ephemeralUploads

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
