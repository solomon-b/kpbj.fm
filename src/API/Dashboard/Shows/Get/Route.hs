module API.Dashboard.Shows.Get.Route where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Filter (Filter)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/shows"
type Route =
  "dashboard"
    :> "shows"
    :> Servant.QueryParam "page" Int64
    :> Servant.QueryParam "q" (Filter Text)
    :> Servant.QueryParam "status" (Filter Shows.Status)
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
