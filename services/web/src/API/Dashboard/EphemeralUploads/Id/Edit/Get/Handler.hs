{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.EphemeralUploads.Id.Edit.Get.Handler (handler, action, UploadEditViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Id.Edit.Get.Templates.Form (ephemeralUploadEditForm)
import API.Links (dashboardEphemeralUploadsLinks)
import API.Types
import App.Config (Environment)
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Domains (audioUploadUrl)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Servant qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the ephemeral upload edit form.
data UploadEditViewData = UploadEditViewData
  { uevUserMetadata :: UserMetadata.Model,
    uevUploadUrl :: Text,
    uevEphemeralUpload :: EphemeralUploads.Model
  }

-- | Business logic: fetch the upload and resolve the upload URL.
action ::
  UserMetadata.Model ->
  EphemeralUploads.Id ->
  ExceptT HandlerError AppM UploadEditViewData
action userMetadata ephemeralUploadId = do
  -- 1. Fetch ephemeral upload
  ephemeralUpload <- fetchEphemeralUpload ephemeralUploadId

  -- 2. Get upload URL (bypasses Cloudflare in production)
  env <- asks (Has.getter @Environment)
  let uploadUrl = audioUploadUrl env

  pure
    UploadEditViewData
      { uevUserMetadata = userMetadata,
        uevUploadUrl = uploadUrl,
        uevEphemeralUpload = ephemeralUpload
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  EphemeralUploads.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler ephemeralUploadId cookie (foldHxReq -> hxRequest) =
  handleRedirectErrors "Ephemeral upload edit" (dashboardEphemeralUploadsLinks.list Nothing) $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff and admins can edit ephemeral uploads." userMetadata
    vd <- action userMetadata ephemeralUploadId
    let content = ephemeralUploadEditForm vd.uevUploadUrl vd.uevEphemeralUpload
    html <- lift $ renderDashboardTemplate hxRequest vd.uevUserMetadata NoShow NavEphemeralUploads Nothing Nothing content
    pure $ Servant.noHeader html

--------------------------------------------------------------------------------
-- Helpers

fetchEphemeralUpload ::
  EphemeralUploads.Id ->
  ExceptT HandlerError AppM EphemeralUploads.Model
fetchEphemeralUpload ephemeralUploadId =
  fromMaybeM (throwNotFound "Ephemeral upload") $
    fromRightM throwDatabaseError $
      execQuery (EphemeralUploads.getEphemeralUploadById ephemeralUploadId)
