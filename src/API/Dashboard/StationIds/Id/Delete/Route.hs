module API.Dashboard.StationIds.Id.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "DELETE /dashboard/station-ids/:station_id"
    ( "dashboard"
        :> "station-ids"
        :> Servant.Capture "station_id" StationIds.Id
        :> Servant.Header "Cookie" Cookie
        :> Servant.Delete '[HTML] (Lucid.Html ())
    )
