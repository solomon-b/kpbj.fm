module API.Shows.Slug.Episode.DiscardDraft.Route where

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

-- | Hard delete route for draft episodes only.
--
-- This allows hosts to discard draft episodes they no longer want to work on.
-- Only draft episodes can be hard deleted - published episodes must be archived
-- by staff instead.
type Route =
  Observability.WithSpan
    "DELETE /shows/:show_slug/episodes/:episode_id/:episode_slug/draft"
    ( "shows"
        :> Servant.Capture "show_slug" Slug
        :> "episodes"
        :> Servant.Capture "episode_id" Episodes.Id
        :> Servant.Capture "episode_slug" Slug
        :> "draft"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Delete '[HTML] (Lucid.Html ())
    )
