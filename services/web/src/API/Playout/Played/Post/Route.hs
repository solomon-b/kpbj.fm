-- | Route definition for POST /api/playout/played.
module API.Playout.Played.Post.Route
  ( Route,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (PlayedRequest)
import Data.Text (Text)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | "POST /api/playout/played"
--
-- Logs a track that has started playing on the stream.
-- Called by Liquidsoap's source.on_track callback.
-- Requires X-Playout-Secret header for authentication.
type Route =
  "api"
    :> "playout"
    :> "played"
    :> Servant.Header "X-Playout-Secret" Text
    :> Servant.ReqBody '[Servant.JSON] PlayedRequest
    :> Servant.PostNoContent
