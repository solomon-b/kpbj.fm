{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Invitations.New.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.New.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/invitations/new.
handler ::
  Satisfies 'Staff RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "New invitation form" apiLinks.rootGet $ do
    lift $ renderDashboardTemplate hxRequest auth.raMetadata NoShow NavInvitations Nothing Nothing template
