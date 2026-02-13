module API.Dashboard.EphemeralUploads.Id.Unflag.Post.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/ephemeral-uploads/:id/unflag"
type Route =
  "dashboard"
    :> "ephemeral-uploads"
    :> Servant.Capture "id" EphemeralUploads.Id
    :> "unflag"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Post '[HTML] (Lucid.Html ())
