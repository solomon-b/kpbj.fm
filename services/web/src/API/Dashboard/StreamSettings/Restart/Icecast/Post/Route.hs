module API.Dashboard.StreamSettings.Restart.Icecast.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/stream-settings/restart/icecast"
type Route =
  "dashboard"
    :> "stream-settings"
    :> "restart"
    :> "icecast"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
