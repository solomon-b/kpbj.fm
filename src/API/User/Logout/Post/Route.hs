module API.User.Logout.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Observability qualified as Observability
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /user/logout"
    ( Servant.AuthProtect "cookie-auth"
        :> "user"
        :> "logout"
        :> Servant.PostAccepted '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] Servant.NoContent)
    )
