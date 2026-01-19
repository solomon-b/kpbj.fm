module API.User.VerifyEmail.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | Route for verifying an email address via token.
--
-- Users arrive here by clicking the verification link in their email.
-- The token query parameter is required.
type Route =
  Observability.WithSpan
    "GET /user/verify-email"
    ( "user"
        :> "verify-email"
        :> Servant.Header "HX-Request" Text
        :> Servant.QueryParam "token" Text
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
