module API.Dashboard.EphemeralUploads.Id.Delete.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/ephemeral-uploads/:ephemeral_upload_id"
type Route =
  "dashboard"
    :> "ephemeral-uploads"
    :> Servant.Capture "ephemeral_upload_id" EphemeralUploads.Id
    :> RequireRole "cookie-auth" 'Host
    :> Servant.Delete '[HTML] (Lucid.Html ())
