-- | Route definition for GET /api/playout/fallback.
module API.Playout.Fallback.Get.Route
  ( Route,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (PlayoutResponse)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | "GET /api/playout/fallback"
--
-- Returns a random ephemeral track URL for fallback playback, or null if none exist.
-- Used by Liquidsoap when no show is scheduled.
type Route =
  "api"
    :> "playout"
    :> "fallback"
    :> Servant.Get '[Servant.JSON] PlayoutResponse
