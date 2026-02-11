module API.Dashboard.Events.Slug.Feature.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/events/:event_id/:event_slug/feature"
type Route =
  "dashboard"
    :> "events"
    :> Servant.Capture "event_id" Events.Id
    :> Servant.Capture "event_slug" Slug
    :> "feature"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
