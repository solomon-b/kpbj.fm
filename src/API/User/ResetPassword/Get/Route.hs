module API.User.ResetPassword.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.PasswordResetTokens (Token)
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /user/reset-password"
    ( "user"
        :> "reset-password"
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.QueryParam "token" Token
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
