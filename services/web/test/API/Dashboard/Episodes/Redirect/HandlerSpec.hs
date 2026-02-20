{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Redirect.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Redirect.Handler (EpisodeRedirectViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
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
        it "returns empty user shows when no shows exist" test_noShowsReturnsEmptyList
        it "admin user sees all active shows" test_adminSeesAllShows
        it "host user sees only their assigned shows" test_hostSeesOnlyOwnShows

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Admin user with no shows in the database gets an empty list.
test_noShowsReturnsEmptyList :: TestDBConfig -> IO ()
test_noShowsReturnsEmptyList cfg = do
  userInsert <- mkUserInsert "redirect-empty" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (userModel, userMetaModel) <- setupUserModels userInsert
        pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> ervUserShows vd `shouldBe` []

-- | Admin user sees all active shows regardless of host assignment.
test_adminSeesAllShows :: TestDBConfig -> IO ()
test_adminSeesAllShows cfg = do
  userInsert <- mkUserInsert "redirect-admin" UserMetadata.Admin
  let showInsert = Shows.Insert "Admin Redirect Show" "admin-redirect-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (userModel, userMetaModel) <- setupUserModels userInsert
        _ <- insertTestShowWithSchedule showInsert defaultScheduleInsert
        pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> ervUserShows vd `shouldSatisfy` (not . null)

-- | Host user only sees shows they are explicitly assigned to, not all shows.
test_hostSeesOnlyOwnShows :: TestDBConfig -> IO ()
test_hostSeesOnlyOwnShows cfg = do
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
        pure (assignedShowId, userModel, userMetaModel)
    (assignedShowId, userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        length (ervUserShows vd) `shouldBe` 1
        let returnedShowIds = map Shows.id (ervUserShows vd)
        returnedShowIds `shouldSatisfy` elem assignedShowId
