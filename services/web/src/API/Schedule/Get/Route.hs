module API.Schedule.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.WeekOffset (WeekOffset)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /schedule"
type Route =
   "schedule"
     :> Servant.QueryParam "week" WeekOffset
     :> Servant.Header "Cookie" Cookie
     :> Servant.Header "HX-Request" HxRequest
     :> Servant.Get '[HTML] (Lucid.Html ())
