module API.Dashboard.Invitations.Delete.Handler (handler) where

--------------------------------------------------------------------------------

import App.Auth.Roles (RoleAuth, Satisfies)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for DELETE /dashboard/invitations/:invitationId.
--
-- Revokes a pending host invitation. Only staff and above can revoke invitations.
-- Returns an OOB banner confirming the revocation.
handler ::
  HostInvitation.Id ->
  Satisfies 'Staff RoleAuth ->
  AppM (Lucid.Html ())
handler invitationId _ =
  handleBannerErrors "Invitation revoke" $ do
    revokeResult <- execQuery (HostInvitation.revokeInvitation invitationId)
    case revokeResult of
      Left dbErr -> throwDatabaseError dbErr
      Right Nothing -> throwNotFound "Invitation"
      Right (Just _) -> pure ()
    pure $ do
      mempty
      renderBanner Success "Invitation Revoked" "The invitation link has been revoked."
