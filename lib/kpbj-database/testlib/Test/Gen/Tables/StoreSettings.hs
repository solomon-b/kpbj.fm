module Test.Gen.Tables.StoreSettings where

--------------------------------------------------------------------------------

import Data.Scientific (Scientific)
import Effects.Database.Tables.StoreSettings qualified as StoreSettings
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genText, genShortText)

--------------------------------------------------------------------------------

-- | Generate a tax rate between 0.00 and 0.20 (0% to 20%).
genTaxRate :: (MonadGen m) => m Scientific
genTaxRate = do
  -- Generate as integer percentage points (0–20), then divide by 100
  pct <- Gen.integral (Range.linear 0 20) :: (MonadGen m) => m Int
  pure $ fromIntegral pct / 100

storeSettingsUpdateGen :: (MonadGen m) => m StoreSettings.UpdateSettings
storeSettingsUpdateGen = do
  usTaxRate <- genTaxRate
  usShipFromName <- genText
  usShipFromAddressLine1 <- genText
  usShipFromCity <- genShortText
  usShipFromState <- genShortText
  usShipFromZip <- Gen.text (Range.singleton 5) Gen.digit
  usShipFromCountry <- genShortText
  usOrderNotificationEmail <- do
    user <- Gen.text (Range.linear 3 10) Gen.alpha
    pure $ user <> "@example.com"
  pure StoreSettings.UpdateSettings {..}
