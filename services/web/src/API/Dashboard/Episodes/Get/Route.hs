module API.Dashboard.Episodes.Get.Route where

--------------------------------------------------------------------------------

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

-- | "GET /dashboard/episodes/:slug"
type Route =
  "dashboard"
    :> "episodes"
    :> Servant.Capture "slug" Slug
    :> Servant.QueryParam "page" Int64
    :> RequireRole "cookie-auth" 'Host
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
