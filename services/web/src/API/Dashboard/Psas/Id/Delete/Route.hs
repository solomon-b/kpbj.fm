module API.Dashboard.Psas.Id.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/psas/:psa_id"
type Route =
  "dashboard"
    :> "psas"
    :> Servant.Capture "psa_id" BreakItems.Id
    :> Servant.Header "Cookie" Cookie
    :> Servant.Delete '[HTML] (Lucid.Html ())
