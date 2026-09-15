module API.Dashboard.Psas.Id.Edit.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/psas/:psa_id/edit"
type Route =
  "dashboard"
    :> "psas"
    :> Servant.Capture "psa_id" BreakItems.Id
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
