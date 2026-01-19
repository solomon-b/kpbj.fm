module API.User.VerifyEmailResend.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.EmailAddress (EmailAddress)
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- | Route for resending the verification email.
--
-- This is an HTMX endpoint that returns a success/error message fragment.
type Route =
  Observability.WithSpan
    "POST /user/verify-email/resend"
    ( "user"
        :> "verify-email"
        :> "resend"
        :> Servant.ReqBody '[Servant.FormUrlEncoded] ResendForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

-- | Form data for resending verification email.
newtype ResendForm = ResendForm
  { rfEmail :: EmailAddress
  }
  deriving stock (Show, Eq)

instance Form.FromForm ResendForm where
  fromForm form = do
    email <- Form.parseUnique "email" form
    pure ResendForm {rfEmail = email}
