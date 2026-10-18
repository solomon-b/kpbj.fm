module API.Dashboard.AirBreaks.Underwriting.Id.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/air-breaks/underwriting/:underwriting_id"
type Route =
  "dashboard"
    :> "air-breaks"
    :> "underwriting"
    :> Servant.Capture "underwriting_id" BreakItems.Id
    :> Servant.Header "Cookie" Cookie
    :> Servant.Delete '[HTML] (Lucid.Html ())
