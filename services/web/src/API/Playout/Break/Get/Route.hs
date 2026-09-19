-- | Route definition for GET /api/playout/break.
module API.Playout.Break.Get.Route
  ( Route,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (BreakResponse)
import Data.Text (Text)
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
--
-- Requires the @X-Playout-Secret@ header, as @POST /api/playout/played@ does.
-- The handler stamps @last_played_at@ to advance the rotation, so this is a
-- write, and nginx proxies every path to the web service. Without the header
-- any caller could spin the rotation and the \"Last Played\" column.
type Route =
  "api"
    :> "playout"
    :> "break"
    :> Servant.Header "X-Playout-Secret" Text
    :> Servant.Get '[Servant.JSON] BreakResponse
