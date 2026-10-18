module API.Dashboard.Shows.Slug.Promos.Id.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/shows/:show_slug/promos/:promo_id"
type Route =
  "dashboard"
    :> "shows"
    :> Servant.Capture "show_slug" Slug
    :> "promos"
    :> Servant.Capture "promo_id" BreakItems.Id
    :> Servant.Header "Cookie" Cookie
    :> Servant.Delete '[HTML] (Lucid.Html ())
