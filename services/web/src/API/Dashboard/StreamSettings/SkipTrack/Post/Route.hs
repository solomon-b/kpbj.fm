module API.Dashboard.StreamSettings.SkipTrack.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/stream-settings/skip-track"
type Route =
  "dashboard"
    :> "stream-settings"
    :> "skip-track"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
