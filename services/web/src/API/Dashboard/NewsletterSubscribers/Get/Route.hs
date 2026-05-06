module API.Dashboard.NewsletterSubscribers.Get.Route where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/newsletter-subscribers"
type Route =
  "dashboard"
    :> "newsletter-subscribers"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.QueryParam "search" Text
    :> Servant.QueryParam "page" Int64
    :> Servant.Get '[HTML] (Lucid.Html ())
