module API.Dashboard.AirBreaks.Timeline.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/air-breaks/timeline"
--
-- The @day@ parameter is @YYYY-MM-DD@. It defaults to today in Pacific.
type Route =
  "dashboard"
    :> "air-breaks"
    :> "timeline"
    :> Servant.QueryParam "day" Text
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
