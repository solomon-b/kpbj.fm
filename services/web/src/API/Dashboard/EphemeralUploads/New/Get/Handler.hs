{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.EphemeralUploads.New.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.New.Get.Templates.Form (ephemeralUploadForm)
import API.Links (apiLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Config (Environment)
import App.Domains (audioUploadUrl)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Ephemeral upload form" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to upload ephemeral clips." userMetadata

    -- 2. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuery Shows.getAllActiveShows
        else execQuery (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    -- 3. Get upload URL (bypasses Cloudflare in production)
    env <- asks (Has.getter @Environment)
    let uploadUrl = audioUploadUrl env

    -- 4. Render the form
    let content = ephemeralUploadForm uploadUrl
    renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavEphemeralUploads Nothing Nothing content
