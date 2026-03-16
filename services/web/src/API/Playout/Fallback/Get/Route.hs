-- | Route definition for GET /api/playout/fallback.
module API.Playout.Fallback.Get.Route
  ( Route,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (FallbackResponse)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | "GET /api/playout/fallback"
--
-- Returns a JSON array of tracks for fallback playback:
-- 1. A randomly selected station ID (if any exist)
-- 2. A randomly selected ephemeral upload
--
-- Returns an empty array if no ephemeral uploads exist or on any database error.
-- Used by Liquidsoap when no show is scheduled.
type Route =
  "api"
    :> "playout"
    :> "fallback"
    :> Servant.Get '[Servant.JSON] FallbackResponse
