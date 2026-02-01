module API.Dashboard.Episodes.Get.Route where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/episodes/:slug"
type Route =
  "dashboard"
    :> "episodes"
    :> Servant.Capture "slug" Slug
    :> Servant.QueryParam "page" Int64
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
