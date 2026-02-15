module API.Dashboard.StreamSettings.StartStream.Post.Route where

import Domain.Types.Cookie (Cookie)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

-- | @POST /dashboard/stream-settings/start-stream@ - Start the entire stream (Icecast then Liquidsoap).
type Route =
  "dashboard"
    :> "stream-settings"
    :> "start-stream"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
