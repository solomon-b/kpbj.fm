module API.Dashboard.AirBreaks.Announcements.Id.Edit.Post.Route where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.BreakItems.Form (BreakItemForm)
import Data.Text (Text)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Servant.Multipart (Mem, MultipartForm)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/air-breaks/announcements/:announcement_id/edit"
type Route =
  "dashboard"
    :> "air-breaks"
    :> "announcements"
    :> Servant.Capture "announcement_id" BreakItems.Id
    :> "edit"
    :> RequireRole "cookie-auth" 'Staff
    :> MultipartForm Mem BreakItemForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
