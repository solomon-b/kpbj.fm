module API.Dashboard.EphemeralUploads.Id.Unflag.Post.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/ephemeral-uploads/:id/unflag"
type Route =
  "dashboard"
    :> "ephemeral-uploads"
    :> Servant.Capture "id" EphemeralUploads.Id
    :> "unflag"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Post '[HTML] (Lucid.Html ())
