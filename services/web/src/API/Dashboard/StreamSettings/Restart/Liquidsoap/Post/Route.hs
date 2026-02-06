module API.Dashboard.StreamSettings.Restart.Liquidsoap.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/stream-settings/restart/liquidsoap"
type Route =
  "dashboard"
    :> "stream-settings"
    :> "restart"
    :> "liquidsoap"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
