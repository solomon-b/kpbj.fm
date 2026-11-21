module API.Dashboard.SitePages.Slug.Revisions.Id.Restore.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.SitePageRevisions qualified as SitePageRevisions
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/site-pages/:slug/revisions/:id/restore"
type Route =
  "dashboard"
    :> "site-pages"
    :> Servant.Capture "slug" Text
    :> "revisions"
    :> Servant.Capture "id" SitePageRevisions.Id
    :> "restore"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
