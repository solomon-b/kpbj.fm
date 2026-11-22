module API.Dashboard.Episodes.Slug.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/episodes/:show_slug/:episode_number"
type Route =
  "dashboard"
    :> "episodes"
    :> Servant.Capture "show_slug" Slug
    :> Servant.Capture "episode_number" Episodes.EpisodeNumber
    :> RequireRole "cookie-auth" 'Host
    :> ReadFlash
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
