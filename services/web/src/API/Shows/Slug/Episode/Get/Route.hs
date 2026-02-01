module API.Shows.Slug.Episode.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /shows/:show_slug/episodes/:episode_number"
--
-- Route for episode detail page using show slug and episode number.
-- URLs are now in the format: @/shows/:show_slug/episodes/:episode_number@
type Route =
  "shows"
    :> Servant.Capture "show_slug" Slug
    :> "episodes"
    :> Servant.Capture "episode_number" Episodes.EpisodeNumber
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Lucid.Html ())
