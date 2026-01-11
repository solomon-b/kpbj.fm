module API.User.Logout.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /user/logout"
    ( Servant.AuthProtect "cookie-auth"
        :> "user"
        :> "logout"
        :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    )
