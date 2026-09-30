module API.Dashboard.Psas.Id.Edit.Post.Route where

--------------------------------------------------------------------------------

import API.Dashboard.BreakItems.Form (BreakItemForm)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Servant ((:>))
import Servant qualified
import Servant.Multipart (Mem, MultipartForm)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/psas/:psa_id/edit"
type Route =
  "dashboard"
    :> "psas"
    :> Servant.Capture "psa_id" BreakItems.Id
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> MultipartForm Mem BreakItemForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
