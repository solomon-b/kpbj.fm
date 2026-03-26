{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Store.Settings.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Settings.Post.Route (SettingsForm (..))
import API.Links (apiLinks, dashboardStoreSettingsLinks, rootLink)
import API.Types (DashboardStoreSettingsRoutes (..), Routes (..))
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth, requireRight)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad.Trans.Except (ExceptT)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.StoreSettings qualified as StoreSettings
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Servant qualified
import Text.Read (readMaybe)
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Parse and validate the settings form, producing an UpdateSettings value.
--
-- The form sends the tax rate as a percentage (e.g. "9.5" for 9.5%).
-- We convert to a fraction (0.095) for storage.
validateSettingsForm :: SettingsForm -> Either Text StoreSettings.UpdateSettings
validateSettingsForm form = do
  taxRatePercent <- case readMaybe @Scientific (Text.unpack form.sfTaxRate) of
    Nothing -> Left ("Invalid tax rate: \"" <> form.sfTaxRate <> "\" is not a valid number")
    Just d | d < 0 || d > 100 -> Left "Tax rate must be between 0 and 100"
    Just d -> Right d
  Right
    StoreSettings.UpdateSettings
      { StoreSettings.usTaxRate = taxRatePercent / 100,
        StoreSettings.usShipFromName = Sanitize.sanitizePlainText form.sfShipFromName,
        StoreSettings.usShipFromAddressLine1 = Sanitize.sanitizePlainText form.sfShipFromAddressLine1,
        StoreSettings.usShipFromCity = Sanitize.sanitizePlainText form.sfShipFromCity,
        StoreSettings.usShipFromState = Sanitize.sanitizePlainText form.sfShipFromState,
        StoreSettings.usShipFromZip = Sanitize.sanitizePlainText form.sfShipFromZip,
        StoreSettings.usShipFromCountry = Sanitize.sanitizePlainText form.sfShipFromCountry,
        StoreSettings.usOrderNotificationEmail = Sanitize.sanitizePlainText form.sfOrderNotificationEmail
      }

-- | All data needed to build the post-update redirect.
data SettingsRedirectData = SettingsRedirectData
  { srdRedirectUrl :: Text,
    srdFlash :: FlashMessage
  }

-- | Business logic: validate form, update settings, build redirect data.
action ::
  User.Model ->
  UserMetadata.Model ->
  SettingsForm ->
  ExceptT HandlerError AppM SettingsRedirectData
action _user _userMetadata form = do
  -- 1. Validate form fields
  update <- requireRight id (validateSettingsForm form)

  -- 2. Persist settings
  fromRightM throwDatabaseError $ execQuery (StoreSettings.updateSettings update)

  -- 3. Build redirect data
  Log.logInfo "Store settings updated successfully" ()

  let redirectUrl = rootLink dashboardStoreSettingsLinks.get
      flash = FlashMessage Success "Settings Saved" "Store settings have been updated successfully."
  pure $ SettingsRedirectData redirectUrl flash

-- | Servant handler: thin glue composing action + building redirect response.
handler ::
  Maybe Cookie ->
  SettingsForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler cookie form =
  handleRedirectErrors "Store settings update" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "You do not have permission to update store settings." userMetadata
    vd <- action user userMetadata form
    pure $ Servant.addHeader vd.srdRedirectUrl $ Servant.addHeader (flashCookie (Just vd.srdFlash)) Servant.NoContent
