module API.Dashboard.CurrentShow.Get.Route where

--------------------------------------------------------------------------------

import App.Dashboard.ShowContext (ShowSection)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/current-show/:section"
--
-- Redirects to a section of the current show.
type Route =
  "dashboard"
    :> "current-show"
    :> Servant.Capture "section" ShowSection
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
