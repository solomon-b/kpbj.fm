module API.Dashboard.Blogs.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Data.Int (Int64)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/blogs"
type Route =
  "dashboard"
    :> "blog"
    :> Servant.Capture "show" Slug
    :> Servant.QueryParam "page" Int64
    :> RequireRole "cookie-auth" 'Host
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
