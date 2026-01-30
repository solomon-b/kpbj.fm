module API.Dashboard.Blogs.Slug.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/blog/:show_slug/:post_id"
type Route =
  "dashboard"
    :> "blog"
    :> Servant.Capture "show_slug" Slug
    :> Servant.Capture "post_id" ShowBlogPosts.Id
    :> Servant.Header "Cookie" Cookie
    :> Servant.Delete '[HTML] (Lucid.Html ())
