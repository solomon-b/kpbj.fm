{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.New.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.New.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuerySpan)
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
  handleHtmlErrors "New show form" apiLinks.rootGet $ do
    -- 1. Require authentication and admin role
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Fetch all users for the host selection dropdown
    eligibleHostsResult <- execQuerySpan (UserMetadata.getAllUsersWithPagination 1000 0)
    eligibleHosts <- case eligibleHostsResult of
      Left err -> throwDatabaseError err
      Right hosts -> pure hosts

    -- 3. Render form
    renderDashboardTemplate hxRequest userMetadata [] Nothing NavShows Nothing Nothing (template eligibleHosts)
