{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationIds.New.Get.Handler (handler, action, StationIdFormViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.StationIds.New.Get.Templates.Form (stationIdUploadForm)
import API.Links (apiLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Config (Environment)
import App.Domains (audioUploadUrl)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
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
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to upload station IDs." userMetadata
    vd <- action user userMetadata
    let content = stationIdUploadForm vd.sifUploadUrl
    lift $ renderDashboardTemplate hxRequest vd.sifUserMetadata vd.sifAllShows vd.sifSelectedShow NavStationIds Nothing Nothing content

--------------------------------------------------------------------------------

-- | All data needed to render the station ID upload form.
data StationIdFormViewData = StationIdFormViewData
  { sifUserMetadata :: UserMetadata.Model,
    sifAllShows :: [Shows.Model],
    sifSelectedShow :: Maybe Shows.Model,
    sifUploadUrl :: Text
  }

-- | Business logic: show sidebar, upload URL.
action ::
  User.Model ->
  UserMetadata.Model ->
  ExceptT HandlerError AppM StationIdFormViewData
action user userMetadata = do
  -- 1. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
      selectedShow = listToMaybe allShows

  -- 2. Get upload URL (bypasses Cloudflare in production)
  env <- asks (Has.getter @Environment)
  let uploadUrl = audioUploadUrl env

  pure
    StationIdFormViewData
      { sifUserMetadata = userMetadata,
        sifAllShows = allShows,
        sifSelectedShow = selectedShow,
        sifUploadUrl = uploadUrl
      }
