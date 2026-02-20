{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.EphemeralUploads.New.Get.Handler (handler, action, UploadNewViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.New.Get.Templates.Form (ephemeralUploadForm)
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

-- | All data needed to render the ephemeral upload new form.
data UploadNewViewData = UploadNewViewData
  { unvUserMetadata :: UserMetadata.Model,
    unvAllShows :: [Shows.Model],
    unvSelectedShow :: Maybe Shows.Model,
    unvUploadUrl :: Text
  }

-- | Business logic: shows fetch, upload URL resolution.
action ::
  User.Model ->
  UserMetadata.Model ->
  ExceptT HandlerError AppM UploadNewViewData
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
    UploadNewViewData
      { unvUserMetadata = userMetadata,
        unvAllShows = allShows,
        unvSelectedShow = selectedShow,
        unvUploadUrl = uploadUrl
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Ephemeral upload form" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to upload ephemeral clips." userMetadata
    vd <- action user userMetadata
    let content = ephemeralUploadForm vd.unvUploadUrl
    lift $ renderDashboardTemplate hxRequest vd.unvUserMetadata vd.unvAllShows vd.unvSelectedShow NavEphemeralUploads Nothing Nothing content
