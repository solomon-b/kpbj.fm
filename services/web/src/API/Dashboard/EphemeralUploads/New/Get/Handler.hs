{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.EphemeralUploads.New.Get.Handler (handler, action, UploadNewViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.New.Get.Templates.Form (ephemeralUploadForm)
import API.Links (apiLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Config (Environment)
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Domains (audioUploadUrl)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Data.Text (Text)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

--------------------------------------------------------------------------------

-- | All data needed to render the ephemeral upload new form.
data UploadNewViewData = UploadNewViewData
  { unvUserMetadata :: UserMetadata.Model,
    unvUploadUrl :: Text
  }

-- | Business logic: upload URL resolution.
action ::
  UserMetadata.Model ->
  ExceptT HandlerError AppM UploadNewViewData
action userMetadata = do
  -- Get upload URL (bypasses Cloudflare in production)
  env <- asks (Has.getter @Environment)
  let uploadUrl = audioUploadUrl env

  pure
    UploadNewViewData
      { unvUserMetadata = userMetadata,
        unvUploadUrl = uploadUrl
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Satisfies 'UserMetadata.Host RoleAuth ->
  Maybe FlashMessage ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors "Ephemeral upload form" apiLinks.rootGet $ do
    vd <- action auth.raMetadata
    let content = ephemeralUploadForm vd.unvUploadUrl
    lift $ renderDashboardTemplate hxRequest mFlash vd.unvUserMetadata NoShow NavEphemeralUploads Nothing Nothing content
