module API.Shows.Slug.Episode.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | Soft delete (archive) route for episodes.
--
-- This is restricted to staff or higher roles. It sets the episode status
-- to 'deleted' but preserves the record in the database. Use this to remove
-- published episodes from public view while maintaining history.
--
-- For discarding draft episodes, hosts should use the DiscardDraft endpoint instead.
type Route =
  Observability.WithSpan
    "DELETE /shows/:show_slug/episodes/:episode_number"
    ( "shows"
        :> Servant.Capture "show_slug" Slug
        :> "episodes"
        :> Servant.Capture "episode_number" Episodes.EpisodeNumber
        :> Servant.Header "Cookie" Cookie
        :> Servant.Delete '[HTML] (Lucid.Html ())
    )
