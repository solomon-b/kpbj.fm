module API.Dashboard.Users.Detail.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/users/:id"
type Route =
  "dashboard"
    :> "users"
    :> Servant.Capture "id" User.Id
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
