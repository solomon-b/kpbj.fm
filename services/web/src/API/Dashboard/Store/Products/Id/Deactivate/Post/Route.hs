module API.Dashboard.Store.Products.Id.Deactivate.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.Products qualified as Products
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/store/products/:productId/deactivate"
type Route =
  "dashboard"
    :> "store"
    :> "products"
    :> Servant.Capture "productId" Products.Id
    :> "deactivate"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
