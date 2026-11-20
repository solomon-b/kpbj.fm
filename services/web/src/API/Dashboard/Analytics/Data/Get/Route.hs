module API.Dashboard.Analytics.Data.Get.Route where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Data.Get.Types (AnalyticsData)
import Data.Text (Text)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)

--------------------------------------------------------------------------------

-- | @GET /dashboard/analytics/data?range=7d@
type Route =
  "dashboard"
    :> "analytics"
    :> "data"
    :> RequireRole "cookie-auth" 'Admin
    :> Servant.QueryParam "range" Text
    :> Servant.Get '[Servant.JSON] AnalyticsData
