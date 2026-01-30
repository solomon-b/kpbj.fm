module API.Dashboard.Users.Unsuspend.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.User qualified as User
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/users/:id/unsuspend"
type Route =
  "dashboard"
    :> "users"
    :> Servant.Capture "id" User.Id
    :> "unsuspend"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
