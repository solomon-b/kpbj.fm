module API.Dashboard.Invitations.Regenerate.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/invitations/:invitationId/regenerate"
type Route =
  "dashboard"
    :> "invitations"
    :> Servant.Capture "invitationId" HostInvitation.Id
    :> "regenerate"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Post
         '[HTML]
         (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
