module API.Dashboard.Store.Orders.Id.Status.Post.Route where

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

-- | Form data for updating order status.
data StatusForm = StatusForm
  { sfStatus :: Text,
    sfTrackingNumber :: Maybe Text
  }
  deriving (Show)

instance FromForm StatusForm where
  fromForm :: Form.Form -> Either Text StatusForm
  fromForm form = do
    status <- Form.parseUnique "status" form
    trackingNumber <- Form.parseMaybe "tracking_number" form
    pure
      StatusForm
        { sfStatus = status,
          sfTrackingNumber = trackingNumber
        }

--------------------------------------------------------------------------------

-- | "POST /dashboard/store/orders/:id/status"
type Route =
  "dashboard"
    :> "store"
    :> "orders"
    :> Servant.Capture "id" Orders.Id
    :> "status"
    :> Servant.Header "Cookie" Cookie
    :> Servant.ReqBody '[Servant.FormUrlEncoded] StatusForm
    :> Servant.Post '[HTML] (Lucid.Html ())
