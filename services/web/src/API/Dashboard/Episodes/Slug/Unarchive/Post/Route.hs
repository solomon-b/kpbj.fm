module API.Dashboard.Episodes.Slug.Unarchive.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/episodes/:show_slug/:episode_number/unarchive"
--
-- Restore route for episodes. It clears @deleted_at@, so the episode returns to
-- the public site. This reverses the Delete route above, and it carries the same
-- staff restriction.
type Route =
  "dashboard"
    :> "episodes"
    :> Servant.Capture "show_slug" Slug
    :> Servant.Capture "episode_number" Episodes.EpisodeNumber
    :> "unarchive"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
