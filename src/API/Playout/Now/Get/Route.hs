-- | Route definition for GET /api/playout/now.
module API.Playout.Now.Get.Route
  ( Route,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (PlayoutResponse)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | "GET /api/playout/now"
--
-- Returns the currently airing episode's audio URL, or null if nothing is scheduled.
-- Used by Liquidsoap for live playback.
type Route =
  "api"
    :> "playout"
    :> "now"
    :> Servant.Get '[Servant.JSON] PlayoutResponse
