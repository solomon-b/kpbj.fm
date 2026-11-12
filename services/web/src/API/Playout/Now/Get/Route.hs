-- | Route definition for GET /api/playout/now.
module API.Playout.Now.Get.Route
  ( Route,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (NowResponse)
import Data.Time (UTCTime)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | "GET /api/playout/now"
--
-- Returns the timeline segment that airs now, resumed at the current second.
-- With @from@, returns the whole segment that starts at that instant. Either
-- way, the response also holds the whole segment after it, in @next@.
-- Used by Liquidsoap for live playback.
type Route =
  "api"
    :> "playout"
    :> "now"
    :> Servant.QueryParam "from" UTCTime
    :> Servant.Get '[Servant.JSON] NowResponse
