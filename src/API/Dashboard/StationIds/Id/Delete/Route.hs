module API.Dashboard.StationIds.Id.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.StationIds qualified as StationIds
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/station-ids/:station_id"
type Route =
  "dashboard"
    :> "station-ids"
    :> Servant.Capture "station_id" StationIds.Id
    :> Servant.Header "Cookie" Cookie
    :> Servant.Delete '[HTML] (Lucid.Html ())
