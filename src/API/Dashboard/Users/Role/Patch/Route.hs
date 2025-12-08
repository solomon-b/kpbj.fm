module API.Dashboard.Users.Role.Patch.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "PATCH /dashboard/users/:id/role"
    ( "dashboard"
        :> "users"
        :> Servant.Capture "id" User.Id
        :> "role"
        :> Servant.Header "Cookie" Cookie
        :> Servant.ReqBody '[Servant.FormUrlEncoded] RoleUpdateForm
        :> Servant.Patch '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

newtype RoleUpdateForm = RoleUpdateForm
  { role :: UserMetadata.UserRole
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromForm)
