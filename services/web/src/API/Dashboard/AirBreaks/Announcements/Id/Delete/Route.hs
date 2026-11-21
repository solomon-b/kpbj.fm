module API.Dashboard.AirBreaks.Announcements.Id.Delete.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/air-breaks/announcements/:announcement_id"
type Route =
  "dashboard"
    :> "air-breaks"
    :> "announcements"
    :> Servant.Capture "announcement_id" BreakItems.Id
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Delete '[HTML] (Lucid.Html ())
