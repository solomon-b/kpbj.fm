module API.Dashboard.StreamSettings.Restart.Icecast.Post.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/stream-settings/restart/icecast"
type Route =
  "dashboard"
    :> "stream-settings"
    :> "restart"
    :> "icecast"
    :> RequireRole "cookie-auth" 'Admin
    :> Servant.Post '[HTML] (Lucid.Html ())
