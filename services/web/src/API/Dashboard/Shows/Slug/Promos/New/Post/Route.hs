module API.Dashboard.Shows.Slug.Promos.New.Post.Route where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.BreakItems.Form (BreakItemForm)
import Data.Text (Text)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Servant.Multipart (Mem, MultipartForm)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/shows/:show_slug/promos/new"
--
-- The promo form is the break item form. It leaves the underwriter empty.
type Route =
  "dashboard"
    :> "shows"
    :> Servant.Capture "show_slug" Slug
    :> "promos"
    :> "new"
    :> RequireRole "cookie-auth" 'Host
    :> MultipartForm Mem BreakItemForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
