module API.Dashboard.Invitations.Id.Edit.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/invitations/:invitationId/edit"
--
-- Returns an inline edit-form row fragment for a pending invitation.
-- The row swaps over the normal row via HTMX outerHTML.
type Route =
  "dashboard"
    :> "invitations"
    :> Servant.Capture "invitationId" HostInvitation.Id
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Get '[HTML] (Lucid.Html ())
