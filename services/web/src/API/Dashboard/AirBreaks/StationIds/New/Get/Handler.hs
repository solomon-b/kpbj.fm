{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.AirBreaks.StationIds.New.Get.Handler (handler, action, StationIdFormViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.StationIds.New.Get.Templates.Form (stationIdUploadForm)
import API.Links (apiLinks)
import API.Types
import App.Config (Environment)
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Domains (audioUploadUrl)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Station ID upload form" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to upload station IDs." userMetadata
    vd <- action userMetadata
    let content = stationIdUploadForm vd.sifUploadUrl
    lift $ renderDashboardTemplate hxRequest vd.sifUserMetadata NoShow NavStationIds Nothing Nothing content

--------------------------------------------------------------------------------

-- | All data needed to render the station ID upload form.
data StationIdFormViewData = StationIdFormViewData
  { sifUserMetadata :: UserMetadata.Model,
    sifUploadUrl :: Text
  }

-- | Business logic: upload URL.
action ::
  UserMetadata.Model ->
  ExceptT HandlerError AppM StationIdFormViewData
action userMetadata = do
  -- Get upload URL (bypasses Cloudflare in production)
  env <- asks (Has.getter @Environment)
  let uploadUrl = audioUploadUrl env

  pure
    StationIdFormViewData
      { sifUserMetadata = userMetadata,
        sifUploadUrl = uploadUrl
      }
