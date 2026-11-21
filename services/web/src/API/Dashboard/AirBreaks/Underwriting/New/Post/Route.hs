module API.Dashboard.AirBreaks.Underwriting.New.Post.Route where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.BreakItems.Form (BreakItemForm)
import Data.Text (Text)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Servant.Multipart (Mem, MultipartForm)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/air-breaks/underwriting/new"
type Route =
  "dashboard"
    :> "air-breaks"
    :> "underwriting"
    :> "new"
    :> RequireRole "cookie-auth" 'Staff
    :> MultipartForm Mem BreakItemForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
