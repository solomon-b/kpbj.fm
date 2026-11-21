module API.Dashboard.AirBreaks.Promos.Id.Approve.Post.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/air-breaks/promos/:promo_id/approve"
type Route =
  "dashboard"
    :> "air-breaks"
    :> "promos"
    :> Servant.Capture "promo_id" BreakItems.Id
    :> "approve"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Post '[HTML] (Lucid.Html ())
