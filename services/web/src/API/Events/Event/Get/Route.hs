module API.Events.Event.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | Route for event with ID and slug (canonical URL)
-- "GET /events/:id/:slug"
type RouteWithSlug =
  "events"
    :> Servant.Capture "id" Events.Id
    :> Servant.Capture "slug" Slug
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))


-- | Route for event with ID only (redirects to canonical)
-- "GET /events/:id"
type RouteWithoutSlug =
  "events"
    :> Servant.Capture "id" Events.Id
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
