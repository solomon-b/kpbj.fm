{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Shows.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Get.Handler (ShowListViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Filter (Filter (..))
import Domain.Types.HxRequest (HxRequest (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (addTestShowHost, insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Shows.Get.Handler" $ do
      describe "action" $ do
        it "empty DB returns no shows" test_emptyDbReturnsNoShows
        it "inserted show appears in results" test_insertedShowAppears
        it "status filter returns only matching shows" test_statusFilterWorks

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Admin user with no shows in DB should see empty shows list.
test_emptyDbReturnsNoShows :: TestDBConfig -> IO ()
test_emptyDbReturnsNoShows cfg = do
  adminInsert <- mkUserInsert "shows-get-empty" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels adminInsert
    (userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing Nothing Nothing IsNotHxRequest

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> null vd.slvShows `shouldBe` True

-- | A show inserted and visible to admin appears in slvShows.
test_insertedShowAppears :: TestDBConfig -> IO ()
test_insertedShowAppears cfg = do
  adminInsert <- mkUserInsert "shows-get-inserted" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels adminInsert
      let showInsert = Shows.Insert "Appears Test Show" "shows-get-appears-test" Nothing Nothing Shows.Active
      (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      -- Add the admin user as a host so the show is visible in filtered queries
      addTestShowHost showId userModel.mId
      pure (userModel, userMetaModel, showId)
    (userModel, userMetaModel, _showId) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing Nothing Nothing IsNotHxRequest

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> not (null vd.slvShows) `shouldBe` True

-- | Status filter returns only shows with matching status.
test_statusFilterWorks :: TestDBConfig -> IO ()
test_statusFilterWorks cfg = do
  adminInsert <- mkUserInsert "shows-get-filter" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels adminInsert
      let activeInsert = Shows.Insert "Active Filter Show" "shows-get-active-filter" Nothing Nothing Shows.Active
          inactiveInsert = Shows.Insert "Inactive Filter Show" "shows-get-inactive-filter" Nothing Nothing Shows.Inactive
      (activeId, _) <- insertTestShowWithSchedule activeInsert defaultScheduleInsert
      (inactiveId, _) <- insertTestShowWithSchedule inactiveInsert defaultScheduleInsert
      addTestShowHost activeId userModel.mId
      addTestShowHost inactiveId userModel.mId
      pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- expectSetupRight dbResult

    activeResult <-
      runExceptT $
        action userModel userMetaModel Nothing Nothing (Just (Filter (Just Shows.Active))) IsNotHxRequest
    inactiveResult <-
      runExceptT $
        action userModel userMetaModel Nothing Nothing (Just (Filter (Just Shows.Inactive))) IsNotHxRequest

    liftIO $ do
      case activeResult of
        Left err -> expectationFailure $ "Expected Right for active filter but got Left: " <> show err
        Right vd -> do
          vd.slvShows `shouldSatisfy` (not . null)
          all (\s -> s.swhiStatus == Shows.Active) vd.slvShows `shouldBe` True
      case inactiveResult of
        Left err -> expectationFailure $ "Expected Right for inactive filter but got Left: " <> show err
        Right vd -> do
          vd.slvShows `shouldSatisfy` (not . null)
          all (\s -> s.swhiStatus == Shows.Inactive) vd.slvShows `shouldBe` True
