module API.Dashboard.Store.Products.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/store/products"
type Route =
  "dashboard"
    :> "store"
    :> "products"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
