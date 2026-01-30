module API.User.ResetPassword.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.PasswordResetTokens (Token)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /user/reset-password"
type Route =
    "user"
      :> "reset-password"
      :> Servant.Header "HX-Request" HxRequest
      :> Servant.QueryParam "token" Token
      :> Servant.Get '[HTML] (Lucid.Html ())
