module API.Dashboard.Store.Products.Id.Edit.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/store/products/:productId/edit"
type Route =
  "dashboard"
    :> "store"
    :> "products"
    :> Servant.Capture "productId" Products.Id
    :> "edit"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
