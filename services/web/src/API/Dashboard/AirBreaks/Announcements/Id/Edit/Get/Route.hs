module API.Dashboard.AirBreaks.Announcements.Id.Edit.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/air-breaks/announcements/:announcement_id/edit"
type Route =
  "dashboard"
    :> "air-breaks"
    :> "announcements"
    :> Servant.Capture "announcement_id" BreakItems.Id
    :> "edit"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
