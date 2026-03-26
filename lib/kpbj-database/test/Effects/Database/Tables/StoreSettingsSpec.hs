module Effects.Database.Tables.StoreSettingsSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.StoreSettings qualified as UUT
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.Tables.StoreSettings (storeSettingsUpdateGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.StoreSettings" $ do
      describe "Queries" $ do
        runs 5 . it "getSettings returns the pre-seeded settings row" $
          hedgehog . prop_getSettings
        runs 10 . it "update-get round-trip: updated fields match" $
          hedgehog . prop_updateGetRoundTrip

--------------------------------------------------------------------------------

-- | The migration seeds a default row with id=1.
prop_getSettings :: TestDBConfig -> PropertyT IO ()
prop_getSettings cfg = do
  arrange (bracketConn cfg) $ do
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        settings <- TRX.statement () UUT.getSettings
        TRX.condemn
        pure settings

      assert $ do
        mSettings <- assertRight result
        settings <- assertJust mSettings
        UUT.ssId settings === 1

prop_updateGetRoundTrip :: TestDBConfig -> PropertyT IO ()
prop_updateGetRoundTrip cfg = do
  arrange (bracketConn cfg) $ do
    updateData <- forAllT storeSettingsUpdateGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        TRX.statement () (UUT.updateSettings updateData)
        settings <- TRX.statement () UUT.getSettings
        TRX.condemn
        pure settings

      assert $ do
        mSettings <- assertRight result
        settings <- assertJust mSettings
        UUT.ssTaxRate settings === UUT.usTaxRate updateData
        UUT.ssShipFromName settings === UUT.usShipFromName updateData
        UUT.ssShipFromAddressLine1 settings === UUT.usShipFromAddressLine1 updateData
        UUT.ssShipFromCity settings === UUT.usShipFromCity updateData
        UUT.ssShipFromState settings === UUT.usShipFromState updateData
        UUT.ssShipFromZip settings === UUT.usShipFromZip updateData
        UUT.ssShipFromCountry settings === UUT.usShipFromCountry updateData
        UUT.ssOrderNotificationEmail settings === UUT.usOrderNotificationEmail updateData
