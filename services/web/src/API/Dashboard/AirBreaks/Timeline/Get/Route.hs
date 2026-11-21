module API.Dashboard.AirBreaks.Timeline.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/air-breaks/timeline"
--
-- The @day@ parameter is @YYYY-MM-DD@. It defaults to today in Pacific.
type Route =
  "dashboard"
    :> "air-breaks"
    :> "timeline"
    :> Servant.QueryParam "day" Text
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
