module API.Dashboard.StreamSettings.Episodes.Search.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/stream-settings/episodes/search?q=..."
type Route =
  "dashboard"
    :> "stream-settings"
    :> "episodes"
    :> "search"
    :> Servant.Header "Cookie" Cookie
    :> Servant.QueryParam "q" Text
    :> Servant.Get '[HTML] (Lucid.Html ())
