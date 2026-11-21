module API.Dashboard.NewsletterSubscribers.Get.Route where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/newsletter-subscribers"
type Route =
  "dashboard"
    :> "newsletter-subscribers"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.QueryParam "search" Text
    :> Servant.QueryParam "page" Int64
    :> Servant.Get '[HTML] (Lucid.Html ())
