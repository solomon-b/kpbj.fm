module API.User.Logout.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /user/logout"
type Route =
  "user"
    :> "logout"
    :> Servant.Header "Cookie" Cookie
    :> Servant.PostAccepted '[HTML] (Servant.Headers '[Servant.Header "Set-Cookie" Text, Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
