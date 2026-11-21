module API.Dashboard.StationBlog.Slug.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Slug (Slug)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/station-blog/:id/:slug"
type Route =
  "dashboard"
    :> "station-blog"
    :> Servant.Capture "id" BlogPosts.Id
    :> Servant.Capture "slug" Slug
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Delete '[HTML] (Lucid.Html ())
