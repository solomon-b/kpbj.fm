module API.Dashboard.NewsletterSubscribers.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/newsletter-subscribers/:subscriberId"
type Route =
  "dashboard"
    :> "newsletter-subscribers"
    :> Servant.Capture "subscriberId" NewsletterSubscribers.Id
    :> Servant.Header "Cookie" Cookie
    :> Servant.Delete '[HTML] (Lucid.Html ())
