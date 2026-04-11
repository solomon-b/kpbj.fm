module API.Store.Checkout.CreateSession.Post.Route where

--------------------------------------------------------------------------------

import API.Store.Types (CreateSessionRequest, CreateSessionResponse)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | "POST /api/store/checkout/create-session"
type Route =
  "api"
    :> "store"
    :> "checkout"
    :> "create-session"
    :> Servant.ReqBody '[Servant.JSON] CreateSessionRequest
    :> Servant.Post '[Servant.JSON] CreateSessionResponse
