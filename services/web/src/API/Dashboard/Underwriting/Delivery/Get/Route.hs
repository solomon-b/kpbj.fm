module API.Dashboard.Underwriting.Delivery.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/underwriting/delivery"
--
-- The @month@ parameter is @YYYY-MM@. It defaults to the current Pacific month.
type Route =
  "dashboard"
    :> "underwriting"
    :> "delivery"
    :> Servant.QueryParam "month" Text
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
