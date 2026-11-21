module API.Dashboard.NewsletterSubscribers.Delete.Route where

--------------------------------------------------------------------------------

import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/newsletter-subscribers/:subscriberId"
type Route =
  "dashboard"
    :> "newsletter-subscribers"
    :> Servant.Capture "subscriberId" NewsletterSubscribers.Id
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.Delete '[HTML] (Lucid.Html ())
