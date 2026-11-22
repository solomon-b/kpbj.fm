module API.Shows.Slug.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /shows/:slug"
type Route =
  "shows"
    :> Servant.Capture "slug" Slug
    :> Servant.QueryParam "page" Int
    :> Servant.Header "Cookie" Cookie
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
