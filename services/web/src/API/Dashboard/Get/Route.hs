module API.Dashboard.Get.Route where

--------------------------------------------------------------------------------

import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard"
type Route =
  "dashboard"
    :> Servant.Get '[HTML] (Lucid.Html ())
