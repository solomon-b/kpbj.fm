module API.Dashboard.Underwriters.New.Post.Route where

--------------------------------------------------------------------------------

import API.Dashboard.Underwriters.Form (UnderwriterForm)
import Domain.Types.Cookie (Cookie)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/underwriters/new"
--
-- Returns the table body and an OOB success banner.
type Route =
  "dashboard"
    :> "underwriters"
    :> "new"
    :> Servant.Header "Cookie" Cookie
    :> Servant.ReqBody '[Servant.FormUrlEncoded] UnderwriterForm
    :> Servant.Post '[HTML] (Lucid.Html ())
