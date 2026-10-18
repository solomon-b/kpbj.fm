module API.Dashboard.AirBreaks.Underwriters.New.Post.Route where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Underwriters.Form (UnderwriterForm)
import Domain.Types.Cookie (Cookie)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/air-breaks/underwriters/new"
--
-- Returns the table body and an OOB success banner.
type Route =
  "dashboard"
    :> "air-breaks"
    :> "underwriters"
    :> "new"
    :> Servant.Header "Cookie" Cookie
    :> Servant.ReqBody '[Servant.FormUrlEncoded] UnderwriterForm
    :> Servant.Post '[HTML] (Lucid.Html ())
