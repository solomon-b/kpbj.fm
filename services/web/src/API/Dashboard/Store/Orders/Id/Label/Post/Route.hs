module API.Dashboard.Store.Orders.Id.Label.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- | Form data for purchasing a shipping label.
data LabelForm = LabelForm
  { lfRateId :: Text,
    lfShipmentId :: Text
  }
  deriving (Show)

instance FromForm LabelForm where
  fromForm :: Form.Form -> Either Text LabelForm
  fromForm form = do
    rateId <- Form.parseUnique "rate_id" form
    shipmentId <- Form.parseUnique "shipment_id" form
    pure
      LabelForm
        { lfRateId = rateId,
          lfShipmentId = shipmentId
        }

--------------------------------------------------------------------------------

-- | "POST /dashboard/store/orders/:id/label"
type Route =
  "dashboard"
    :> "store"
    :> "orders"
    :> Servant.Capture "id" Orders.Id
    :> "label"
    :> RequireRole "cookie-auth" 'Staff
    :> Servant.ReqBody '[Servant.FormUrlEncoded] LabelForm
    :> Servant.Post '[HTML] (Lucid.Html ())
