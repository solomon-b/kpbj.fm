module Effects.Database.Tables.StreamSettingsSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.StreamSettings qualified as UUT
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.StreamSettings" $ do
      describe "Queries" $ do
        runs 5 . it "getStreamSettings: returns the singleton row" $
          hedgehog . prop_getStreamSettings

      describe "Mutations" $ do
        runs 5 . it "updateStreamSettings: updates fields and records updated_by" $
          hedgehog . prop_updateStreamSettings

--------------------------------------------------------------------------------
-- Query tests

-- | getStreamSettings: returns the singleton pre-seeded row.
prop_getStreamSettings :: TestDBConfig -> PropertyT IO ()
prop_getStreamSettings cfg = do
  arrange (bracketConn cfg) $ do
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
        settings <- TRX.statement () UUT.getStreamSettings
        TRX.condemn
        pure settings

      assert $ do
        mSettings <- assertRight result
        -- Singleton row should exist from migration
        _ <- assertJust mSettings
        pure ()

--------------------------------------------------------------------------------
-- Mutation tests

-- | updateStreamSettings: updates fields and records the user who made the change.
prop_updateStreamSettings :: TestDBConfig -> PropertyT IO ()
prop_updateStreamSettings cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let update =
              UUT.Update
                { UUT.ssuStreamUrl = "https://stream.example.com/live",
                  UUT.ssuMetadataUrl = "https://stream.example.com/metadata"
                }
        mUpdated <- TRX.statement () (UUT.updateStreamSettings userId update)
        TRX.condemn
        pure (userId, mUpdated)

      assert $ do
        (userId, mUpdated) <- assertRight result
        updated <- assertJust mUpdated
        UUT.ssStreamUrl updated === "https://stream.example.com/live"
        UUT.ssMetadataUrl updated === "https://stream.example.com/metadata"
        UUT.ssUpdatedBy updated === Just userId
        pure ()
