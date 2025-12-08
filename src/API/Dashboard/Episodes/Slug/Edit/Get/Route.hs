module API.Dashboard.Episodes.Slug.Edit.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard/episodes/:show_slug/:episode_id/:slug/edit"
    ( "dashboard"
        :> "episodes"
        :> Servant.Capture "show_slug" Slug
        :> Servant.Capture "episode_id" Episodes.Id
        :> Servant.Capture "slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Push-Url" Text, Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    )
