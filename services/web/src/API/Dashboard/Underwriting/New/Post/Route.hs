module API.Dashboard.Underwriting.New.Post.Route where

--------------------------------------------------------------------------------

import API.Dashboard.BreakItems.Form (BreakItemForm)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Servant ((:>))
import Servant qualified
import Servant.Multipart (Mem, MultipartForm)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/underwriting/new"
type Route =
  "dashboard"
    :> "underwriting"
    :> "new"
    :> Servant.Header "Cookie" Cookie
    :> MultipartForm Mem BreakItemForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
