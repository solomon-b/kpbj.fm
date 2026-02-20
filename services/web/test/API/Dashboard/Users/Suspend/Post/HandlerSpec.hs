{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.Suspend.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Suspend.Post.Handler (SuspendResult (..), action)
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
    describe "API.Dashboard.Users.Suspend.Post.Handler" $ do
      describe "action" $ do
        it "returns TargetUserNotFound for missing user" test_notFoundForMissingUser
        it "suspends an existing user successfully" test_suspendsExistingUser
        it "suspended user has the correct user ID" test_suspendedUserHasCorrectId

--------------------------------------------------------------------------------

-- | Returns TargetUserNotFound when the target does not exist.
test_notFoundForMissingUser :: TestDBConfig -> IO ()
test_notFoundForMissingUser cfg = do
  bracketAppM cfg $ do
    result <- runExceptT $ action (User.Id nonExistentId) "test reason"

    liftIO $ case result of
      Right (TargetUserNotFound uid) -> uid `shouldBe` User.Id nonExistentId
      Right (SuspendSuccess _) -> expectationFailure "Expected TargetUserNotFound but got SuspendSuccess"
      Right (SuspendFailed _) -> expectationFailure "Expected TargetUserNotFound but got SuspendFailed"
      Left err -> expectationFailure $ "Expected Right TargetUserNotFound but got Left: " <> show err

-- | Suspends an existing user and returns SuspendSuccess.
test_suspendsExistingUser :: TestDBConfig -> IO ()
test_suspendsExistingUser cfg = do
  targetInsert <- mkUserInsert "usr-suspend-target-success" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action targetId "Violated community guidelines"

    liftIO $ case result of
      Right (SuspendSuccess _) -> pure ()
      Right (TargetUserNotFound _) -> expectationFailure "Expected SuspendSuccess but got TargetUserNotFound"
      Right (SuspendFailed err) -> expectationFailure $ "Expected SuspendSuccess but got SuspendFailed: " <> show err
      Left err -> expectationFailure $ "Expected Right SuspendSuccess but got Left: " <> show err

-- | The suspended UserWithMetadata inside SuspendSuccess has the correct user ID.
test_suspendedUserHasCorrectId :: TestDBConfig -> IO ()
test_suspendedUserHasCorrectId cfg = do
  targetInsert <- mkUserInsert "usr-suspend-target-userid" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action targetId "spam"

    liftIO $ case result of
      Right (SuspendSuccess updatedUser) -> updatedUser.uwmUserId `shouldBe` targetId
      Right (TargetUserNotFound _) -> expectationFailure "Expected SuspendSuccess but got TargetUserNotFound"
      Right (SuspendFailed err) -> expectationFailure $ "Expected SuspendSuccess but got SuspendFailed: " <> show err
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
