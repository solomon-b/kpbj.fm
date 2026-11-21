module API.Dashboard.Store.Products.Id.Deactivate.Post.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/store/products/:productId/deactivate"
type Route =
  "dashboard"
    :> "store"
    :> "products"
    :> Servant.Capture "productId" Products.Id
    :> "deactivate"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Post '[HTML] (Lucid.Html ())
