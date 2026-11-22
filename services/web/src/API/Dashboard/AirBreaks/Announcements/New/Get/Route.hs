module API.Dashboard.AirBreaks.Announcements.New.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/air-breaks/announcements/new"
type Route =
  "dashboard"
    :> "air-breaks"
    :> "announcements"
    :> "new"
    :> RequireRole "cookie-auth" 'Staff
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
