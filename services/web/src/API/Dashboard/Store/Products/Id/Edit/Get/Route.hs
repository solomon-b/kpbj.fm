module API.Dashboard.Store.Products.Id.Edit.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.Products qualified as Products
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/store/products/:productId/edit"
type Route =
  "dashboard"
    :> "store"
    :> "products"
    :> Servant.Capture "productId" Products.Id
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
