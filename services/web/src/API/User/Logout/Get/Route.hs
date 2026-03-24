module API.User.Logout.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /user/logout"
type Route =
  "user"
    :> "logout"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Get '[HTML] (Lucid.Html ())
