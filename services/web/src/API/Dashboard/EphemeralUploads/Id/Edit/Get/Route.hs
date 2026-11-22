module API.Dashboard.EphemeralUploads.Id.Edit.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Data.Text (Text)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/ephemeral-uploads/:ephemeral_upload_id/edit"
type Route =
  "dashboard"
    :> "ephemeral-uploads"
    :> Servant.Capture "ephemeral_upload_id" EphemeralUploads.Id
    :> "edit"
    :> RequireRole "cookie-auth" 'Staff
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
