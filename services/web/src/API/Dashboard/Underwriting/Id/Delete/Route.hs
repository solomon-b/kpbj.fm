module API.Dashboard.Underwriting.Id.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/underwriting/:underwriting_id"
type Route =
  "dashboard"
    :> "underwriting"
    :> Servant.Capture "underwriting_id" BreakItems.Id
    :> Servant.Header "Cookie" Cookie
    :> Servant.Delete '[HTML] (Lucid.Html ())
