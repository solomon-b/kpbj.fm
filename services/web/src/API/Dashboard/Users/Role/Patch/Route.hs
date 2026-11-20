module API.Dashboard.Users.Role.Patch.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm)

--------------------------------------------------------------------------------

-- | "PATCH /dashboard/users/:id/role"
type Route =
  "dashboard"
    :> "users"
    :> Servant.Capture "id" User.Id
    :> "role"
    :> RequireRole "cookie-auth" 'UserMetadata.Admin
    :> Servant.ReqBody '[Servant.FormUrlEncoded] RoleUpdateForm
    :> Servant.Patch '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

newtype RoleUpdateForm = RoleUpdateForm
  { role :: UserMetadata.UserRole
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromForm)
