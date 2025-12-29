module API.Dashboard.Events.Slug.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Events qualified as Events
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "DELETE /dashboard/events/:event_id/:event_slug"
    ( "dashboard"
        :> "events"
        :> Servant.Capture "event_id" Events.Id
        :> Servant.Capture "event_slug" Slug
        :> Servant.Header "Cookie" Cookie
        :> Servant.Delete '[HTML] (Lucid.Html ())
    )
