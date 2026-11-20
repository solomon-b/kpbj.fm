module API.Dashboard.StreamSettings.StartStream.Post.Route where

import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

-- | @POST /dashboard/stream-settings/start-stream@ - Start the entire stream (Icecast then Liquidsoap).
type Route =
  "dashboard"
    :> "stream-settings"
    :> "start-stream"
    :> RequireRole "cookie-auth" 'Admin
    :> Servant.Post '[HTML] (Lucid.Html ())
