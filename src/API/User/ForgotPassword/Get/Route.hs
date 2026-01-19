module API.User.ForgotPassword.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.HxRequest (HxRequest)
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /user/forgot-password"
    ( "user"
        :> "forgot-password"
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
