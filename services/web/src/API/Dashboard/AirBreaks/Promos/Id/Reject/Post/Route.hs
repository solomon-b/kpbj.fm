module API.Dashboard.AirBreaks.Promos.Id.Reject.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
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
    :> Servant.Header "Cookie" Cookie
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
