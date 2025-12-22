module API.Dashboard.Blogs.Get.Route where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard/blogs"
    ( "dashboard"
        :> "blog"
        :> Servant.Capture "show" Slug
        :> Servant.QueryParam "page" Int64
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
