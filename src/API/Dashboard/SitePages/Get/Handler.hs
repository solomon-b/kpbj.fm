{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.SitePages.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Data.Either (fromRight)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.SitePages qualified as SitePages
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
  handleHtmlErrors "Site pages list" apiLinks.rootGet $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuerySpan Shows.getAllActiveShows
        else execQuerySpan (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult

    -- 3. Fetch all site pages
    pagesResult <- execQuerySpan SitePages.getAllPages
    pages <- case pagesResult of
      Left err -> throwDatabaseError err
      Right ps -> pure ps

    -- 4. Render response
    renderDashboardTemplate hxRequest userMetadata allShows Nothing NavSitePages Nothing Nothing (template pages)
