-- | Route definition for GET /api/playout/break.
module API.Playout.Break.Get.Route
  ( Route,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (BreakResponse)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | "GET /api/playout/break"
--
-- Returns a JSON array of tracks for one break window:
--
-- 1. A randomly selected station ID, if any exist
-- 2. The PSAs and advertisement spots that fit in the rest of the window
--
-- Returns an empty array when no break is due at the next slot boundary, and
-- on any database error. Liquidsoap asks at @:28@ and @:58@ and plays on
-- without cutting anything when the array is empty.
type Route =
  "api"
    :> "playout"
    :> "break"
    :> Servant.Get '[Servant.JSON] BreakResponse
