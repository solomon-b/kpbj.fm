module API.Dashboard.Shows.Slug.Restore.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.Slug (Slug)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/shows/:slug/restore"
type Route =
  "dashboard"
    :> "shows"
    :> Servant.Capture "slug" Slug
    :> "restore"
    :> RequireRole "cookie-auth" 'Admin
    :> Servant.Post '[HTML] (Lucid.Html ())
