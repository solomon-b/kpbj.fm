module API.User.Logout.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /user/logout"
type Route =
  Servant.AuthProtect "cookie-auth"
    :> "user"
    :> "logout"
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
