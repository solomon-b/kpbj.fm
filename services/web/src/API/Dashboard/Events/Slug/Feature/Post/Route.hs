module API.Dashboard.Events.Slug.Feature.Post.Route where

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

-- | "POST /dashboard/events/:event_id/:event_slug/feature"
type Route =
  "dashboard"
    :> "events"
    :> Servant.Capture "event_id" Events.Id
    :> Servant.Capture "event_slug" Slug
    :> "feature"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Post '[HTML] (Lucid.Html ())
