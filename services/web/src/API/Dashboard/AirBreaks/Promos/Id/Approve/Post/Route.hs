module API.Dashboard.AirBreaks.Promos.Id.Approve.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/air-breaks/promos/:promo_id/approve"
type Route =
  "dashboard"
    :> "air-breaks"
    :> "promos"
    :> Servant.Capture "promo_id" BreakItems.Id
    :> "approve"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
