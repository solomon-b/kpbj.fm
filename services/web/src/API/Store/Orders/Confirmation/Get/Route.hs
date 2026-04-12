module API.Store.Orders.Confirmation.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /store/orders/:order-number/confirmation?session_id=..."
type Route =
  "store"
    :> "orders"
    :> Servant.Capture "order-number" Text
    :> "confirmation"
    :> Servant.QueryParam "session_id" Text
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
