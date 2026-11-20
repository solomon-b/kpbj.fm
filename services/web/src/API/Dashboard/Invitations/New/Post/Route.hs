module API.Dashboard.Invitations.New.Post.Route where

--------------------------------------------------------------------------------

import App.FormField (emailField)
import Data.Bifunctor (first)
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.EmailAddress (EmailAddress)
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

-- | Form data for creating a new host invitation.
data NewInvitationForm = NewInvitationForm
  { nifRecipientEmail :: EmailAddress,
    nifSchedulesJson :: Text,
    nifScheduleStartDate :: Text
  }
  deriving (Show)

instance FromMultipart Mem NewInvitationForm where
  fromMultipart multipartData = do
    recipientEmail <- first Text.unpack . emailField $ fromRight "" (lookupInput "recipient_email" multipartData)
    pure
      NewInvitationForm
        { nifRecipientEmail = recipientEmail,
          nifSchedulesJson = fromRight "[]" (lookupInput "schedules_json" multipartData),
          nifScheduleStartDate = fromRight "" (lookupInput "schedule_start_date" multipartData)
        }

--------------------------------------------------------------------------------

-- | "POST /dashboard/invitations/new"
type Route =
  "dashboard"
    :> "invitations"
    :> "new"
    :> Servant.Header "Cookie" Cookie
    :> MultipartForm Mem NewInvitationForm
    :> Servant.Post
         '[HTML]
         (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
