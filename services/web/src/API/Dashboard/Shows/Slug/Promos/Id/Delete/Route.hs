module API.Dashboard.Shows.Slug.Promos.Id.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Slug (Slug)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/shows/:show_slug/promos/:promo_id"
type Route =
  "dashboard"
    :> "shows"
    :> Servant.Capture "show_slug" Slug
    :> "promos"
    :> Servant.Capture "promo_id" BreakItems.Id
    :> RequireRole "cookie-auth" 'Host
    :> Servant.Delete '[HTML] (Lucid.Html ())
