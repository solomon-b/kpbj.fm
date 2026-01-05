module API.Dashboard.Episodes.Slug.Publish.Post.Route where

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

type Route =
  Observability.WithSpan
    "POST /dashboard/episodes/:show_slug/:episode_number/publish"
    ( "dashboard"
        :> "episodes"
        :> Servant.Capture "show_slug" Slug
        :> Servant.Capture "episode_number" Episodes.EpisodeNumber
        :> "publish"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Post '[HTML] (Lucid.Html ())
    )
