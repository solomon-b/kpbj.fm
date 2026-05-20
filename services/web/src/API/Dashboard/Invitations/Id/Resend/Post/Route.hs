module API.Dashboard.Invitations.Id.Resend.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/invitations/:invitationId/resend"
--
-- Re-fires the invitation email with the existing token (no DB change).
-- Returns an OOB success banner.
type Route =
  "dashboard"
    :> "invitations"
    :> Servant.Capture "invitationId" HostInvitation.Id
    :> "resend"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
