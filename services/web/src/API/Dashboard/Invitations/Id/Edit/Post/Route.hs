module API.Dashboard.Invitations.Id.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Either (fromRight)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Servant ((:>))
import Servant qualified
import Servant.Multipart
  ( FromMultipart (..),
    Mem,
    MultipartForm,
    lookupInput,
  )
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | Form data for the edit of a pending host invitation.
data EditInvitationForm = EditInvitationForm
  { eifRecipientEmail :: Text,
    eifSchedulesJson :: Text,
    eifScheduleStartDate :: Text
  }
  deriving (Show)

instance FromMultipart Mem EditInvitationForm where
  fromMultipart multipartData =
    pure
      EditInvitationForm
        { eifRecipientEmail = fromRight "" (lookupInput "recipient_email" multipartData),
          eifSchedulesJson = fromRight "[]" (lookupInput "schedules_json" multipartData),
          eifScheduleStartDate = fromRight "" (lookupInput "schedule_start_date" multipartData)
        }

--------------------------------------------------------------------------------

-- | "POST /dashboard/invitations/:invitationId/edit"
--
-- Updates the recipient, the schedule, and the start date of a pending
-- invitation.
type Route =
  "dashboard"
    :> "invitations"
    :> Servant.Capture "invitationId" HostInvitation.Id
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> MultipartForm Mem EditInvitationForm
    :> Servant.Post
         '[HTML]
         (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
