{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Redirect.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Redirect.Handler (action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Maybe (isJust)
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
    describe "API.Dashboard.Episodes.Redirect.Handler" $ do
      describe "action" $ do
        it "returns no show when no shows exist" test_noShowsReturnsNothing
        it "admin user gets an active show" test_adminGetsActiveShow
        it "host user gets only their assigned show" test_hostGetsOwnShow

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Admin user with no shows in the database gets no show.
test_noShowsReturnsNothing :: TestDBConfig -> IO ()
test_noShowsReturnsNothing cfg = do
  userInsert <- mkUserInsert "redirect-empty" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (_, userMetaModel) <- setupUserModels userInsert
        pure userMetaModel
    userMetaModel <- expectSetupRight dbResult

    result <- runExceptT $ action Nothing userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right mShow -> fmap Shows.id mShow `shouldBe` Nothing

-- | Admin user gets an active show regardless of host assignment.
test_adminGetsActiveShow :: TestDBConfig -> IO ()
test_adminGetsActiveShow cfg = do
  userInsert <- mkUserInsert "redirect-admin" UserMetadata.Admin
  let showInsert = Shows.Insert "Admin Redirect Show" "admin-redirect-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (_, userMetaModel) <- setupUserModels userInsert
        _ <- insertTestShowWithSchedule showInsert defaultScheduleInsert
        pure userMetaModel
    userMetaModel <- expectSetupRight dbResult

    result <- runExceptT $ action Nothing userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right mShow -> mShow `shouldSatisfy` isJust

-- | Host user gets a show they are assigned to, never an unassigned show.
test_hostGetsOwnShow :: TestDBConfig -> IO ()
test_hostGetsOwnShow cfg = do
  userInsert <- mkUserInsert "redirect-host" UserMetadata.Host
  let assignedShowInsert = Shows.Insert "Host Assigned Show" "host-assigned-show" Nothing Nothing Shows.Active
      otherShowInsert = Shows.Insert "Other Redirect Show" "other-redirect-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (userModel, userMetaModel) <- setupUserModels userInsert
        (assignedShowId, _) <- insertTestShowWithSchedule assignedShowInsert defaultScheduleInsert
        (_, _) <- insertTestShowWithSchedule otherShowInsert defaultScheduleInsert
        addTestShowHost assignedShowId userModel.mId
        pure (assignedShowId, userMetaModel)
    (assignedShowId, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action Nothing userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right mShow -> fmap Shows.id mShow `shouldBe` Just assignedShowId
