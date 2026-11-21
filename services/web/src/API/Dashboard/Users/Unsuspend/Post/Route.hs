module API.Dashboard.Users.Unsuspend.Post.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/users/:id/unsuspend"
type Route =
  "dashboard"
    :> "users"
    :> Servant.Capture "id" User.Id
    :> "unsuspend"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Post '[HTML] (Lucid.Html ())
