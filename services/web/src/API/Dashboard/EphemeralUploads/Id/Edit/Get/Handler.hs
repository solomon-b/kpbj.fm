{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.EphemeralUploads.Id.Edit.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Id.Edit.Get.Templates.Form (ephemeralUploadEditForm)
import API.Links (dashboardEphemeralUploadsLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Config (Environment)
import App.Domains (audioUploadUrl)
import App.Handler.Combinators (requireAuth)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Servant qualified

--------------------------------------------------------------------------------

handler ::
  EphemeralUploads.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler ephemeralUploadId cookie (foldHxReq -> hxRequest) =
  handleRedirectErrors "Ephemeral upload edit" (dashboardEphemeralUploadsLinks.list Nothing) $ do
    -- 1. Require authentication and staff/admin role
    (user, userMetadata) <- requireAuth cookie

    -- 2. Fetch ephemeral upload
    ephemeralUpload <- fetchEphemeralUpload ephemeralUploadId

    -- 3. Check authorization: must be staff/admin
    let isStaffOrAdmin = UserMetadata.isStaffOrHigher userMetadata.mUserRole
    unless isStaffOrAdmin $
      throwNotAuthorized "Only staff and admins can edit ephemeral uploads." (Just userMetadata.mUserRole)

    -- 4. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuery Shows.getAllActiveShows
        else execQuery (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    -- 5. Get upload URL (bypasses Cloudflare in production)
    env <- asks (Has.getter @Environment)
    let uploadUrl = audioUploadUrl env

    -- 6. Render the edit form
    let content = ephemeralUploadEditForm uploadUrl ephemeralUpload
    html <- renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavEphemeralUploads Nothing Nothing content
    pure $ Servant.noHeader html

--------------------------------------------------------------------------------
-- Helpers

fetchEphemeralUpload ::
  EphemeralUploads.Id ->
  AppM EphemeralUploads.Model
fetchEphemeralUpload ephemeralUploadId =
  execQuery (EphemeralUploads.getEphemeralUploadById ephemeralUploadId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Ephemeral upload"
    Right (Just ephemeralUpload) -> pure ephemeralUpload
