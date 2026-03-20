{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Invitations.New.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.New.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/invitations/new.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "New invitation form" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can create invitations." userMetadata
    lift $ renderDashboardTemplate hxRequest userMetadata [] Nothing NavInvitations Nothing Nothing template
