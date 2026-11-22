module API.Dashboard.Events.Slug.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/events/:id/:slug"
type Route =
  "dashboard"
    :> "events"
    :> Servant.Capture "id" Events.Id
    :> Servant.Capture "slug" Slug
    :> RequireRole "cookie-auth" 'Staff
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
