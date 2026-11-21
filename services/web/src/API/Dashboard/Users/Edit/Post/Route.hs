module API.Dashboard.Users.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Servant.Multipart (Mem, MultipartData, MultipartForm)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/users/:id/edit"
type Route =
  "dashboard"
    :> "users"
    :> Servant.Capture "id" User.Id
    :> "edit"
    :> RequireRole "cookie-auth" 'Staff
    :> MultipartForm Mem (MultipartData Mem)
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
