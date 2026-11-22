module API.Dashboard.Shows.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.Filter (Filter)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/shows"
type Route =
  "dashboard"
    :> "shows"
    :> Servant.QueryParam "page" Int64
    :> Servant.QueryParam "q" (Filter Text)
    :> Servant.QueryParam "status" (Filter Shows.Status)
    :> Servant.QueryFlag "deleted"
    :> RequireRole "cookie-auth" 'Staff
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
