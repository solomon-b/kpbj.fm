module API.Dashboard.Users.Delete.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/users/:id"
type Route =
  "dashboard"
    :> "users"
    :> Servant.Capture "id" User.Id
    :> RequireRole "cookie-auth" 'Admin
    :> Servant.Delete '[HTML] (Lucid.Html ())
