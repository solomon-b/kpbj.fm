module API.Dashboard.Shows.Slug.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/shows/:showId/:showSlug"
type Route =
  "dashboard"
    :> "shows"
    :> Servant.Capture "showId" Shows.Id
    :> Servant.Capture "showSlug" Slug
    :> Servant.QueryParam "page" Int
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
