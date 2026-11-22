module API.Dashboard.Invitations.Get.Route where

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

-- | "GET /dashboard/invitations"
type Route =
  "dashboard"
    :> "invitations"
    :> RequireRole "cookie-auth" 'Staff
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
