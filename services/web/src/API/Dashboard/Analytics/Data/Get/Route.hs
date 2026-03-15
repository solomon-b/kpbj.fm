module API.Dashboard.Analytics.Data.Get.Route where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Data.Get.Types (AnalyticsData)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | @GET /dashboard/analytics/data?range=7d@
type Route =
  "dashboard"
    :> "analytics"
    :> "data"
    :> Servant.Header "Cookie" Cookie
    :> Servant.QueryParam "range" Text
    :> Servant.Get '[Servant.JSON] AnalyticsData
