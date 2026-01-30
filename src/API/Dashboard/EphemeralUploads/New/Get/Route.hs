module API.Dashboard.EphemeralUploads.New.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/ephemeral-uploads/new"
type Route =
    "dashboard"
      :> "ephemeral-uploads"
      :> "new"
      :> Servant.Header "Cookie" Cookie
      :> Servant.Header "HX-Request" HxRequest
      :> Servant.Get '[HTML] (Lucid.Html ())
