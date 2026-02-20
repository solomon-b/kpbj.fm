{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Shows.New.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.New.Get.Handler (ShowNewGetViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Shows.New.Get.Handler" $ do
      describe "action" $ do
        it "returns inserted users as eligible hosts" test_returnsEligibleHosts
        it "returned metadata matches input" test_returnsUserMetadata

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Inserted users appear in the eligible hosts list.
test_returnsEligibleHosts :: TestDBConfig -> IO ()
test_returnsEligibleHosts cfg = do
  staffInsert <- mkUserInsert "shows-new-staff" UserMetadata.Staff
  hostInsert1 <- mkUserInsert "shows-new-host1" UserMetadata.Host
  hostInsert2 <- mkUserInsert "shows-new-host2" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      staffId <- insertTestUser staffInsert
      _ <- insertTestUser hostInsert1
      _ <- insertTestUser hostInsert2
      TRX.statement () (UserMetadata.getUserMetadata staffId) >>= maybe (error "metadata not found") pure
    userMetaModel <- expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd ->
        -- At least the 3 users we inserted should appear
        (length vd.sngvEligibleHosts >= 3) `shouldBe` True

-- | The returned metadata record matches the metadata passed to action.
test_returnsUserMetadata :: TestDBConfig -> IO ()
test_returnsUserMetadata cfg = do
  staffInsert <- mkUserInsert "shows-new-meta" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      staffId <- insertTestUser staffInsert
      TRX.statement () (UserMetadata.getUserMetadata staffId) >>= maybe (error "metadata not found") pure
    userMetaModel <- expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd ->
        UserMetadata.mId vd.sngvUserMetadata `shouldBe` UserMetadata.mId userMetaModel
