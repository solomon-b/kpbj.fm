module API.Dashboard.Profile.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Servant.Multipart (Mem, MultipartData, MultipartForm)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/profile/edit"
type Route =
  "dashboard"
    :> "profile"
    :> "edit"
    :> RequireRole "cookie-auth" 'User
    :> Servant.Header "HX-Request" HxRequest
    :> MultipartForm Mem (MultipartData Mem)
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
