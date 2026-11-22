module API.Dashboard.SitePages.Slug.Revisions.Id.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Data.Text (Text)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.SitePageRevisions qualified as SitePageRevisions
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/site-pages/:slug/revisions/:id"
type Route =
  "dashboard"
    :> "site-pages"
    :> Servant.Capture "slug" Text
    :> "revisions"
    :> Servant.Capture "id" SitePageRevisions.Id
    :> RequireRole "cookie-auth" 'Staff
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
