module API.Shows.Slug.Blog.Get.Route where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /shows/:slug/blog"
type Route =
  "shows"
    :> Servant.Capture "slug" Slug
    :> "blog"
    :> Servant.QueryParam "page" Int64
    :> Servant.QueryParam "tag" Text
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
