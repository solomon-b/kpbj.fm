module API.Dashboard.AirBreaks.Promos.Id.Reject.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.UserMetadata (UserRole (..))
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

-- | "POST /dashboard/air-breaks/promos/:promo_id/reject"
type Route =
  "dashboard"
    :> "air-breaks"
    :> "promos"
    :> Servant.Capture "promo_id" BreakItems.Id
    :> "reject"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.ReqBody '[Servant.FormUrlEncoded] RejectForm
    :> Servant.Post '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

-- | Form data for rejecting a show promo.
newtype RejectForm = RejectForm
  { -- | Why staff rejected the promo. The host reads this.
    rfNote :: Text
  }
  deriving stock (Generic, Show)

instance FormUrlEncoded.FromForm RejectForm where
  fromForm f =
    RejectForm
      <$> FormUrlEncoded.parseUnique "note" f
