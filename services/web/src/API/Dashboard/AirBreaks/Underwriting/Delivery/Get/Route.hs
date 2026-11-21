module API.Dashboard.AirBreaks.Underwriting.Delivery.Get.Route where

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

-- | "GET /dashboard/air-breaks/underwriting/delivery"
--
-- The @month@ parameter is @YYYY-MM@. It defaults to the current Pacific month.
type Route =
  "dashboard"
    :> "air-breaks"
    :> "underwriting"
    :> "delivery"
    :> Servant.QueryParam "month" Text
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
