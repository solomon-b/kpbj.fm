module API.Dashboard.Events.Slug.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/events/:event_id/:event_slug"
type Route =
  "dashboard"
    :> "events"
    :> Servant.Capture "event_id" Events.Id
    :> Servant.Capture "event_slug" Slug
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Delete '[HTML] (Lucid.Html ())
