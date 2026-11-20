module API.Dashboard.Store.Settings.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole)
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- | "POST /dashboard/store/settings"
type Route =
  "dashboard"
    :> "store"
    :> "settings"
    :> RequireRole "cookie-auth" 'Admin
    :> Servant.ReqBody '[Servant.FormUrlEncoded] SettingsForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)

--------------------------------------------------------------------------------

-- | Form data for updating store settings.
data SettingsForm = SettingsForm
  { sfTaxRate :: Text,
    sfShipFromName :: Text,
    sfShipFromAddressLine1 :: Text,
    sfShipFromCity :: Text,
    sfShipFromState :: Text,
    sfShipFromZip :: Text,
    sfShipFromCountry :: Text,
    sfOrderNotificationEmail :: Text
  }
  deriving (Show)

instance FromForm SettingsForm where
  fromForm :: Form.Form -> Either Text SettingsForm
  fromForm form = do
    taxRate <- Form.parseUnique "tax_rate" form
    shipFromName <- Form.parseUnique "ship_from_name" form
    shipFromAddressLine1 <- Form.parseUnique "ship_from_address_line1" form
    shipFromCity <- Form.parseUnique "ship_from_city" form
    shipFromState <- Form.parseUnique "ship_from_state" form
    shipFromZip <- Form.parseUnique "ship_from_zip" form
    shipFromCountry <- Form.parseUnique "ship_from_country" form
    orderNotificationEmail <- Form.parseUnique "order_notification_email" form
    pure
      SettingsForm
        { sfTaxRate = taxRate,
          sfShipFromName = shipFromName,
          sfShipFromAddressLine1 = shipFromAddressLine1,
          sfShipFromCity = shipFromCity,
          sfShipFromState = shipFromState,
          sfShipFromZip = shipFromZip,
          sfShipFromCountry = shipFromCountry,
          sfOrderNotificationEmail = orderNotificationEmail
        }
