{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Profile.Edit.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Profile.Edit.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth)
import App.Handler.Error (handleHtmlErrors, throwUserSuspended)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad (when)
import Control.Monad.Reader (asks)
import Data.Either (fromRight)
import Data.Has (getter)
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Profile edit" apiLinks.rootGet $ do
    -- 1. Require authentication
    (user, userMetadata) <- requireAuth cookie

    -- 2. Check not suspended
    when (UserMetadata.isSuspended userMetadata) throwUserSuspended

    -- 3. Get storage backend for URL construction
    storageBackend <- asks getter

    -- 4. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuerySpan Shows.getAllActiveShows
        else execQuerySpan (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavSettings Nothing Nothing (template storageBackend user userMetadata)
