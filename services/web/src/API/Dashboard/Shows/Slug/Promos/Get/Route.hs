module API.Dashboard.Shows.Slug.Promos.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/shows/:show_slug/promos"
type Route =
  "dashboard"
    :> "shows"
    :> Servant.Capture "show_slug" Slug
    :> "promos"
    :> RequireRole "cookie-auth" 'Host
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
