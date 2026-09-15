module API.Dashboard.AdSpots.Id.Edit.Post.Route where

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

-- | "POST /dashboard/ad-spots/:ad_spot_id/edit"
type Route =
  "dashboard"
    :> "ad-spots"
    :> Servant.Capture "ad_spot_id" BreakItems.Id
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> MultipartForm Mem BreakItemForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
