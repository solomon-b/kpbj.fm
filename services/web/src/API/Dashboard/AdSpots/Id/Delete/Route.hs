module API.Dashboard.AdSpots.Id.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/ad-spots/:ad_spot_id"
type Route =
  "dashboard"
    :> "ad-spots"
    :> Servant.Capture "ad_spot_id" BreakItems.Id
    :> Servant.Header "Cookie" Cookie
    :> Servant.Delete '[HTML] (Lucid.Html ())
