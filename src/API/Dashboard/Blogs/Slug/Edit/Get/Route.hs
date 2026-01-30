module API.Dashboard.Blogs.Slug.Edit.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/blog/:show_slug/:post_id/edit"
type Route =
  "dashboard"
    :> "blog"
    :> Servant.Capture "show_slug" Slug
    :> Servant.Capture "post_id" ShowBlogPosts.Id
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
