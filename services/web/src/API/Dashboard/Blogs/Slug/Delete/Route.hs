module API.Dashboard.Blogs.Slug.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/blog/:show_slug/:post_id"
type Route =
  "dashboard"
    :> "blog"
    :> Servant.Capture "show_slug" Slug
    :> Servant.Capture "post_id" ShowBlogPosts.Id
    :> RequireRole "cookie-auth" 'Host
    :> Servant.Delete '[HTML] (Lucid.Html ())
