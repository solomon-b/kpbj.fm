module API.User.ForgotPassword.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.HxRequest (HxRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /user/forgot-password"
type Route =
  "user"
    :> "forgot-password"
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
