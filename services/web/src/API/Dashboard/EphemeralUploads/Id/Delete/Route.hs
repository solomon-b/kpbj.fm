module API.Dashboard.EphemeralUploads.Id.Delete.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "DELETE /dashboard/ephemeral-uploads/:ephemeral_upload_id"
type Route =
  "dashboard"
    :> "ephemeral-uploads"
    :> Servant.Capture "ephemeral_upload_id" EphemeralUploads.Id
    :> Servant.Header "Cookie" Cookie
    :> Servant.Delete '[HTML] (Lucid.Html ())
