module API.Dashboard.Invitations.Id.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- | Form data for updating an invitation's recipient email.
newtype EditInvitationForm = EditInvitationForm
  { eifRecipientEmail :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Form.FromForm EditInvitationForm where
  fromForm form = EditInvitationForm <$> Form.parseUnique "recipient_email" form

--------------------------------------------------------------------------------

-- | "POST /dashboard/invitations/:invitationId/edit"
--
-- Inline-row HTMX endpoint: updates the recipient email of a pending
-- invitation and returns the normal-row fragment plus an OOB success banner.
type Route =
  "dashboard"
    :> "invitations"
    :> Servant.Capture "invitationId" HostInvitation.Id
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> Servant.ReqBody '[Servant.FormUrlEncoded] EditInvitationForm
    :> Servant.Post '[HTML] (Lucid.Html ())
