module API.Store.ShippingRates.Post.Route where

--------------------------------------------------------------------------------

import API.Store.Types (ShippingRateRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /api/store/shipping-rates"
type Route =
  "api"
    :> "store"
    :> "shipping-rates"
    :> Servant.ReqBody '[Servant.JSON] ShippingRateRequest
    :> Servant.Post '[HTML] (Lucid.Html ())
