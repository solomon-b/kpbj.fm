module API.User.VerifyEmailSent.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.EmailAddress (EmailAddress)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /user/verify-email/sent"
--
-- Route for the "check your email" page shown after registration.
-- This page tells users to check their email for a verification link.
-- The email query parameter is optional but helps personalize the message.
type Route =
  "user"
    :> "verify-email"
    :> "sent"
    :> Servant.Header "HX-Request" Text
    :> Servant.QueryParam "email" EmailAddress
    :> Servant.Get '[HTML] (Lucid.Html ())
