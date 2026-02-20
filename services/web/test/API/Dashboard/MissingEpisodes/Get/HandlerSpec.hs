{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.MissingEpisodes.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.MissingEpisodes.Get.Handler (MissingEpisodesViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.MissingEpisodes.Get.Handler" $ do
      describe "action" $ do
        it "staff user on empty DB gets empty missing episodes" test_staffEmptyDbNoMissing
        it "admin user returns Right" test_adminUserReturnsRight

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | A staff user on a DB with no scheduled shows in the next 7 days
-- gets an empty missing-episodes list.
test_staffEmptyDbNoMissing :: TestDBConfig -> IO ()
test_staffEmptyDbNoMissing cfg = do
  staffInsert <- mkUserInsert "me-get-staff" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels staffInsert
    (userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> vd.mevMissingEpisodes `shouldBe` []

-- | An admin user can access the missing-episodes page without error.
test_adminUserReturnsRight :: TestDBConfig -> IO ()
test_adminUserReturnsRight cfg = do
  adminInsert <- mkUserInsert "me-get-admin" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels adminInsert

    (userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()
