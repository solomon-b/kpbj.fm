module API.Dashboard.Users.Delete.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Delete.Handler (DeleteResult (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, nonExistentId)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Users.Delete.Handler" $ do
      describe "action" $ do
        it "returns TargetUserNotFound for missing user" test_notFoundForMissingUser
        it "soft deletes an existing user" test_softDeletesExistingUser
        it "returns DeleteSuccess with correct user ID" test_deleteSuccessHasCorrectId

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Returns TargetUserNotFound for a user ID that does not exist.
test_notFoundForMissingUser :: TestDBConfig -> IO ()
test_notFoundForMissingUser cfg = do
  bracketAppM cfg $ do
    result <- runExceptT $ action (User.Id nonExistentId)
    liftIO $ case result of
      Right (TargetUserNotFound uid) -> uid `shouldBe` User.Id nonExistentId
      Right (DeleteSuccess _ _) -> expectationFailure "Expected TargetUserNotFound but got DeleteSuccess"
      Left err -> expectationFailure $ "Expected Right TargetUserNotFound but got Left: " <> show err

-- | Soft deletes a user that exists, returning DeleteSuccess.
test_softDeletesExistingUser :: TestDBConfig -> IO ()
test_softDeletesExistingUser cfg = do
  targetInsert <- mkUserInsert "usr-delete-target-soft" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action targetId

    liftIO $ case result of
      Right (DeleteSuccess _ _) -> pure ()
      Right (TargetUserNotFound _) -> expectationFailure "Expected DeleteSuccess but got TargetUserNotFound"
      Left err -> expectationFailure $ "Expected Right DeleteSuccess but got Left: " <> show err

-- | DeleteSuccess carries the correct target user ID.
test_deleteSuccessHasCorrectId :: TestDBConfig -> IO ()
test_deleteSuccessHasCorrectId cfg = do
  targetInsert <- mkUserInsert "usr-delete-target-id" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action targetId

    liftIO $ case result of
      Right (DeleteSuccess uid _email) -> uid `shouldBe` targetId
      Right (TargetUserNotFound _) -> expectationFailure "Expected DeleteSuccess but got TargetUserNotFound"
      Left err -> expectationFailure $ "Expected Right DeleteSuccess but got Left: " <> show err
