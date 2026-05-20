module API.Dashboard.Invitations.Id.Row.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/invitations/:invitationId/row"
--
-- Returns the normal (non-edit) HTML row fragment for a single invitation.
-- Used by HTMX cancel actions to restore the row after entering edit mode.
type Route =
  "dashboard"
    :> "invitations"
    :> Servant.Capture "invitationId" HostInvitation.Id
    :> "row"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Get '[HTML] (Lucid.Html ())
