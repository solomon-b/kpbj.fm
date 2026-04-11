module API.Webhooks.Stripe.Post.Route where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.Text (Text)
import Servant ((:>))
import Servant qualified
import Stripe.Servant.RawBody (RawBody)

--------------------------------------------------------------------------------

-- | "POST /api/webhooks/stripe"
type Route =
  "api"
    :> "webhooks"
    :> "stripe"
    :> Servant.Header "Stripe-Signature" Text
    :> Servant.ReqBody '[RawBody] ByteString
    :> Servant.Post '[Servant.JSON] Servant.NoContent
