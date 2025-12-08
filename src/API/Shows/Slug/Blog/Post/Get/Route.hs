module API.Shows.Slug.Blog.Post.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | Route for show blog post with show ID, post ID and slug (canonical URL)
type RouteWithSlug =
  Observability.WithSpan
    "GET /shows/:show_id/blog/:post_id/:slug"
    ( "shows"
        :> Servant.Capture "show_id" Shows.Id
        :> "blog"
        :> Servant.Capture "post_id" ShowBlogPosts.Id
        :> Servant.Capture "slug" Slug
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    )

-- | Route for show blog post with show ID and post ID only (redirects to canonical)
type RouteWithoutSlug =
  Observability.WithSpan
    "GET /shows/:show_id/blog/:post_id"
    ( "shows"
        :> Servant.Capture "show_id" Shows.Id
        :> "blog"
        :> Servant.Capture "post_id" ShowBlogPosts.Id
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    )
