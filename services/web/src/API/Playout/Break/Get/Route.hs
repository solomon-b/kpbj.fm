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
-- Returns a JSON array of tracks for the break window that ends at the next
-- slot boundary. The tracks come from the stored daily plan:
--
-- 1. A station ID
-- 2. At a show break, the underwriting announcements planned for it
-- 3. The PSAs that fit in the rest of the window
--
-- A repeated request for the same boundary returns the same tracks. Returns
-- an empty array when the plan has nothing for the boundary, and on any
-- database error. Liquidsoap asks at @:28@ and @:58@ and plays on without
-- cutting anything when the array is empty.
--
-- Requires the @X-Playout-Secret@ header, as @POST /api/playout/played@ does.
-- The first request of a day builds that day's plan, so this is a write, and
-- nginx proxies every path to the web service.
type Route =
  "api"
    :> "playout"
    :> "break"
    :> Servant.Header "X-Playout-Secret" Text
    :> Servant.Get '[Servant.JSON] BreakResponse
