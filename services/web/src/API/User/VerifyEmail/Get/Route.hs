module API.User.VerifyEmail.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /user/verify-email"
--
-- Route for verifying an email address via token.
-- Users arrive here by clicking the verification link in their email.
-- On successful verification, the user is automatically logged in via
-- session cookie and redirected to the home page.
type Route =
  "user"
    :> "verify-email"
    :> Servant.RemoteHost
    :> Servant.Header "User-Agent" Text
    :> Servant.Header "HX-Request" Text
    :> Servant.QueryParam "token" Text
    :> Servant.Get '[HTML] (Lucid.Html ())
