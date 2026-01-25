module API.Dashboard.SitePages.Slug.Revisions.Id.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.SitePageRevisions qualified as SitePageRevisions
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard/site-pages/:slug/revisions/:id"
    ( "dashboard"
        :> "site-pages"
        :> Servant.Capture "slug" Text
        :> "revisions"
        :> Servant.Capture "id" SitePageRevisions.Id
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
