module API.Dashboard.Shows.Slug.Episode.New.Get.Route where

--------------------------------------------------------------------------------

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
    "GET /dashboard/shows/:show_slug/episodes/new"
    ( "dashboard"
        :> "shows"
        :> Servant.Capture "show_slug" Slug
        :> "episodes"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
