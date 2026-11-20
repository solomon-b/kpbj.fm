module API.Dashboard.Shows.Slug.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Slug (Slug)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/shows/:slug"
type Route =
  "dashboard"
    :> "shows"
    :> Servant.Capture "slug" Slug
    :> RequireRole "cookie-auth" 'Admin
    :> Servant.Delete '[HTML] (Lucid.Html ())
