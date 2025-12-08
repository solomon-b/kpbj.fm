module API.Dashboard.StationBlog.Get.Route where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard/station-blog"
    ( "dashboard"
        :> "station-blog"
        :> Servant.QueryParam "page" Int64
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
