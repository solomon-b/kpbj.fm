module API.Dashboard.Shows.Slug.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/shows/:showId/:showSlug"
type Route =
  "dashboard"
    :> "shows"
    :> Servant.Capture "showId" Shows.Id
    :> Servant.Capture "showSlug" Slug
    :> Servant.QueryParam "page" Int
    :> RequireRole "cookie-auth" 'Host
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
