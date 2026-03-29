module API.Store.Cart.Validate.Post.Route where

--------------------------------------------------------------------------------

import API.Store.Types (CartRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /api/store/cart/validate"
type Route =
  "api"
    :> "store"
    :> "cart"
    :> "validate"
    :> Servant.ReqBody '[Servant.JSON] CartRequest
    :> Servant.Post '[HTML] (Lucid.Html ())
