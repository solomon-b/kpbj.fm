module API.Dashboard.Analytics.Get.Route where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Get.Types (AnalyticsRange)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | @GET /dashboard/analytics?range=7d@
type Route =
  "dashboard"
    :> "analytics"
    :> RequireRole "cookie-auth" 'Admin
    :> Servant.QueryParam "range" AnalyticsRange
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
