module API.Dashboard.AirBreaks.StationIds.Id.Delete.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/air-breaks/station-ids/:station_id"
type Route =
  "dashboard"
    :> "air-breaks"
    :> "station-ids"
    :> Servant.Capture "station_id" StationIds.Id
    :> RequireRole "cookie-auth" 'Host
    :> Servant.Delete '[HTML] (Lucid.Html ())
