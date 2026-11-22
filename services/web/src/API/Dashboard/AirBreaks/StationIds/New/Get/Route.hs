module API.Dashboard.AirBreaks.StationIds.New.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/air-breaks/station-ids/new"
type Route =
  "dashboard"
    :> "air-breaks"
    :> "station-ids"
    :> "new"
    :> RequireRole "cookie-auth" 'Host
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
