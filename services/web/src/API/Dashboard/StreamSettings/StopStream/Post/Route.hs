module API.Dashboard.StreamSettings.StopStream.Post.Route where

import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

-- | @POST /dashboard/stream-settings/stop-stream@ - Stop the entire stream (Liquidsoap then Icecast).
type Route =
  "dashboard"
    :> "stream-settings"
    :> "stop-stream"
    :> RequireRole "cookie-auth" 'Admin
    :> Servant.Post '[HTML] (Lucid.Html ())
