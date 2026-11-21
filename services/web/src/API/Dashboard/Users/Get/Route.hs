module API.Dashboard.Users.Get.Route where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.Filter (Filter)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.UserSortBy (UserSortBy)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/users"
type Route =
  "dashboard"
    :> "users"
    :> Servant.QueryParam "page" Int64
    :> Servant.QueryParam "q" (Filter Text)
    :> Servant.QueryParam "role" (Filter UserMetadata.UserRole)
    :> Servant.QueryParam "sort" (Filter UserSortBy)
    :> RequireRole "cookie-auth" 'UserMetadata.Staff
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
