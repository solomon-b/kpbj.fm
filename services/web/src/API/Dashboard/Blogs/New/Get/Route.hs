module API.Dashboard.Blogs.New.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/blog/:show_slug/new"
type Route =
  "dashboard"
    :> "blog"
    :> Servant.Capture "show_slug" Slug
    :> "new"
    :> RequireRole "cookie-auth" 'Host
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
