module API.User.Logout.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /user/logout"
type Route =
  Servant.AuthProtect "cookie-auth"
    :> "user"
    :> "logout"
    :> Servant.PostAccepted '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
