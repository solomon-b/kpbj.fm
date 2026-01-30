module API.Dashboard.SitePages.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/site-pages"
type Route =
    "dashboard"
      :> "site-pages"
      :> Servant.Header "Cookie" Cookie
      :> Servant.Header "HX-Request" HxRequest
      :> Servant.Get '[HTML] (Lucid.Html ())
