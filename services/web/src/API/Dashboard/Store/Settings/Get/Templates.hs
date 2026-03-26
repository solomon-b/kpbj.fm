{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Store.Settings.Get.Templates (template) where

--------------------------------------------------------------------------------

import API.Links (dashboardStoreSettingsLinks, rootLink)
import API.Types (DashboardStoreSettingsRoutes (..))
import Data.Scientific (FPFormat (..), Scientific, formatScientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Tables.StoreSettings qualified as StoreSettings
import Lucid qualified
import Lucid.Form.Builder

--------------------------------------------------------------------------------

-- | Store settings form template.
--
-- Displays all mutable store_settings fields and submits via HTMX POST.
-- The form targets /dashboard/store/settings (POST).
template :: StoreSettings.Model -> Lucid.Html ()
template settings =
  renderForm config form
  where
    postUrl :: Text
    postUrl = rootLink dashboardStoreSettingsLinks.post

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = postUrl,
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcHtmxSwap = Just "innerHTML"
        }

    -- Display the stored fraction as a percentage for the form (0.095 → "9.500")
    taxRateAsPercent :: Scientific
    taxRateAsPercent = settings.ssTaxRate * 100

    taxRateText :: Text
    taxRateText = Text.pack $ formatScientific Fixed (Just 3) taxRateAsPercent

    form :: FormBuilder
    form = do
      formTitle "STORE SETTINGS"

      -- Tax section
      section "TAX" $ do
        textField "tax_rate" $ do
          label "Tax Rate %"
          placeholder "e.g. 9.500"
          hint "Enter the tax rate as a percentage (e.g. 9.5 for 9.5%)"
          value taxRateText
          pattern' "[0-9]+(\\.[0-9]{1,3})?"
          required

      -- Shipping origin section
      section "SHIP FROM ADDRESS" $ do
        textField "ship_from_name" $ do
          label "Ship From Name"
          placeholder "e.g. KPBJ 95.9FM"
          value settings.ssShipFromName
          required
          maxLength 200

        textField "ship_from_address_line1" $ do
          label "Address Line 1"
          placeholder "e.g. 1234 Main St"
          value settings.ssShipFromAddressLine1
          required
          maxLength 200

        textField "ship_from_city" $ do
          label "City"
          placeholder "e.g. Shadow Hills"
          value settings.ssShipFromCity
          required
          maxLength 100

        textField "ship_from_state" $ do
          label "State"
          placeholder "e.g. CA"
          value settings.ssShipFromState
          required
          maxLength 50

        textField "ship_from_zip" $ do
          label "ZIP Code"
          placeholder "e.g. 91040"
          value settings.ssShipFromZip
          required
          maxLength 20

        textField "ship_from_country" $ do
          label "Country"
          placeholder "e.g. US"
          value settings.ssShipFromCountry
          required
          maxLength 10

      -- Notifications section
      section "NOTIFICATIONS" $ do
        textField "order_notification_email" $ do
          label "Order Notification Email"
          placeholder "e.g. store@kpbj.fm"
          value settings.ssOrderNotificationEmail
          required
          maxLength 254

      submitButton "SAVE SETTINGS"
