module API.Dashboard.Invitations.Delete.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/invitations/:invitationId"
type Route =
  "dashboard"
    :> "invitations"
    :> Servant.Capture "invitationId" HostInvitation.Id
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Delete '[HTML] (Lucid.Html ())
