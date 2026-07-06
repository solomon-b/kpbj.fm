module API.Dashboard.Shows.Slug.Restore.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/shows/:slug/restore"
type Route =
  "dashboard"
    :> "shows"
    :> Servant.Capture "slug" Slug
    :> "restore"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
