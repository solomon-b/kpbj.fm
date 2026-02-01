{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StreamSettings.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StreamSettings.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Data.Either (fromRight)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.StreamSettings qualified as StreamSettings
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Stream settings" apiLinks.rootGet $ do
    -- 1. Require authentication and admin role
    (user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can access stream settings." userMetadata

    -- 2. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuery Shows.getAllActiveShows
        else execQuery (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult

    -- 3. Fetch stream settings
    settingsResult <- execQuery StreamSettings.getStreamSettings
    settings <- case settingsResult of
      Left err -> throwDatabaseError err
      Right Nothing -> throwNotFound "Stream settings not found in database."
      Right (Just s) -> pure s

    -- 4. Render response
    renderDashboardTemplate hxRequest userMetadata allShows Nothing NavStreamSettings Nothing Nothing (template settings Nothing)
