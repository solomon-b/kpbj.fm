module API.Dashboard.MissingEpisodes.Past.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Domain.Attendance (AttendanceRange)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | @GET /dashboard/missing-episodes/past?range=90d@
type Route =
  "dashboard"
    :> "missing-episodes"
    :> "past"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.QueryParam "range" AttendanceRange
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
