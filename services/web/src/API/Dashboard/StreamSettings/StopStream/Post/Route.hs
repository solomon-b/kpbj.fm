module API.Dashboard.StreamSettings.StopStream.Post.Route where

import Domain.Types.Cookie (Cookie)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

-- | @POST /dashboard/stream-settings/stop-stream@ - Stop the entire stream (Liquidsoap then Icecast).
type Route =
  "dashboard"
    :> "stream-settings"
    :> "stop-stream"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
