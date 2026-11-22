{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Invitations.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardInvitationsLinks, rootLink)
import API.Types (DashboardInvitationsRoutes (..), Routes (..))
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.BaseUrl qualified
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.Button ((&))
import Component.Button qualified as Button
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/invitations.
handler ::
  Satisfies 'Staff RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Invitation list" apiLinks.rootGet $ do
    invitations <- fromRightM throwDatabaseError $ execQuery HostInvitation.getAllWithCreator
    appBaseUrl <- lift App.BaseUrl.baseUrl
    lift $
      renderDashboardTemplate
        hxRequest
        auth.raMetadata
        NoShow
        NavInvitations
        Nothing
        (Just actionButton)
        (template appBaseUrl invitations)

-- | Action button for creating a new invitation.
actionButton :: Lucid.Html ()
actionButton =
  Button.render (Button.defaults & Button.primary & Button.dashboardLink newInvitationUrl) "New Invitation"
  where
    newInvitationUrl = rootLink dashboardInvitationsLinks.newGet
