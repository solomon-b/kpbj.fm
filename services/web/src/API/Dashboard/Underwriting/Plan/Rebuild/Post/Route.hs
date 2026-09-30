module API.Dashboard.Underwriting.Plan.Rebuild.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/underwriting/plan/rebuild"
type Route =
  "dashboard"
    :> "underwriting"
    :> "plan"
    :> "rebuild"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
