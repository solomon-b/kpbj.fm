module API.Dashboard.Events.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Data.Int (Int64)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/events"
type Route =
  "dashboard"
    :> "events"
    :> Servant.QueryParam "page" Int64
    :> RequireRole "cookie-auth" 'Staff
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
