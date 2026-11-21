module API.Dashboard.AirBreaks.Underwriters.Id.Edit.Post.Route where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Underwriters.Form (UnderwriterForm)
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/air-breaks/underwriters/:underwriter_id/edit"
--
-- Returns the table body and an OOB success banner.
type Route =
  "dashboard"
    :> "air-breaks"
    :> "underwriters"
    :> Servant.Capture "underwriter_id" Underwriters.Id
    :> "edit"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.ReqBody '[Servant.FormUrlEncoded] UnderwriterForm
    :> Servant.Post '[HTML] (Lucid.Html ())
