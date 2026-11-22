module API.Dashboard.Store.Orders.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/store/orders"
type Route =
  "dashboard"
    :> "store"
    :> "orders"
    :> RequireRole "cookie-auth" 'Staff
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
