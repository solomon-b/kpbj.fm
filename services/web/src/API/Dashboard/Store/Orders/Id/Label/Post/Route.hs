module API.Dashboard.Store.Orders.Id.Label.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.Orders qualified as Orders
import Lucid qualified
import Servant ((:>))
import Servant qualified
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
    :> Servant.Header "Cookie" Cookie
    :> Servant.ReqBody '[Servant.FormUrlEncoded] LabelForm
    :> Servant.Post '[HTML] (Lucid.Html ())
