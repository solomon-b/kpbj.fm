module API.Dashboard.CurrentShow.Get.Route where

--------------------------------------------------------------------------------

import App.Dashboard.ShowContext (ShowSection)
import App.Flash (ReadFlash)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/current-show/:section"
--
-- Redirects to a section of the current show. The handler reads the
-- @dashboard_show@ cookie from the @Cookie@ header.
type Route =
  "dashboard"
    :> "current-show"
    :> Servant.Capture "section" ShowSection
    :> RequireRole "cookie-auth" 'Host
    :> Servant.Header "Cookie" Cookie
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
