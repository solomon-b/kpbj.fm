module API.Blog.Post.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /blog/:id/:slug" - Route for blog post with ID and slug (canonical URL)
type RouteWithSlug =
  "blog"
    :> Servant.Capture "id" BlogPosts.Id
    :> Servant.Capture "slug" Slug
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))

-- | "GET /blog/:id" - Route for blog post with ID only (redirects to canonical)
type RouteWithoutSlug =
  "blog"
    :> Servant.Capture "id" BlogPosts.Id
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
