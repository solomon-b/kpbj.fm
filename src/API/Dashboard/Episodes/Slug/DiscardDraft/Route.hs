module API.Dashboard.Episodes.Slug.DiscardDraft.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/episodes/:show_slug/:episode_number/draft"
--
-- Hard delete route for draft episodes only.
-- This allows hosts to discard draft episodes they no longer want to work on.
-- Only draft episodes can be hard deleted - published episodes must be archived
-- by staff instead.
type Route =
  "dashboard"
    :> "episodes"
    :> Servant.Capture "show_slug" Slug
    :> Servant.Capture "episode_number" Episodes.EpisodeNumber
    :> "draft"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Delete '[HTML] (Lucid.Html ())
