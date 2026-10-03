module API.Dashboard.Underwriters.Id.Edit.Post.Route where

--------------------------------------------------------------------------------

import API.Dashboard.Underwriters.Form (UnderwriterForm)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/underwriters/:underwriter_id/edit"
--
-- Returns the table body and an OOB success banner.
type Route =
  "dashboard"
    :> "underwriters"
    :> Servant.Capture "underwriter_id" Underwriters.Id
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> Servant.ReqBody '[Servant.FormUrlEncoded] UnderwriterForm
    :> Servant.Post '[HTML] (Lucid.Html ())
