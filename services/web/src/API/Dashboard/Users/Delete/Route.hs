module API.Dashboard.Users.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.User qualified as User
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/users/:id"
type Route =
  "dashboard"
    :> "users"
    :> Servant.Capture "id" User.Id
    :> Servant.Header "Cookie" Cookie
    :> Servant.Delete '[HTML] (Lucid.Html ())
