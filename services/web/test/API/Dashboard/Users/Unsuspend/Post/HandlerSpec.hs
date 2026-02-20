{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.Unsuspend.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Unsuspend.Post.Handler (UnsuspendResult (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
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
    describe "API.Dashboard.Users.Unsuspend.Post.Handler" $ do
      describe "action" $ do
        it "returns TargetUserNotFound for missing user" test_notFoundForMissingUser
        it "returns TargetUserNotFound for a non-suspended user" test_notFoundForNonSuspendedUser
        it "unsuspends a suspended user successfully" test_unsuspendsSuspendedUser
        it "UnsuspendSuccess carries the correct target user ID" test_successCarriesCorrectId

--------------------------------------------------------------------------------

-- | Returns TargetUserNotFound when the user ID does not exist at all.
test_notFoundForMissingUser :: TestDBConfig -> IO ()
test_notFoundForMissingUser cfg = do
  bracketAppM cfg $ do
    result <- runExceptT $ action (User.Id nonExistentId)

    liftIO $ case result of
      Right (TargetUserNotFound uid) -> uid `shouldBe` User.Id nonExistentId
      Right (UnsuspendSuccess _) -> expectationFailure "Expected TargetUserNotFound but got UnsuspendSuccess"
      Right (UnsuspendFailed _) -> expectationFailure "Expected TargetUserNotFound but got UnsuspendFailed"
      Left err -> expectationFailure $ "Expected Right TargetUserNotFound but got Left: " <> show err

-- | Returns TargetUserNotFound for a user who is not currently suspended.
--
-- The unsuspendUser query requires suspended_at IS NOT NULL, so an active
-- (non-suspended) user returns Nothing from the MaybeT chain.
test_notFoundForNonSuspendedUser :: TestDBConfig -> IO ()
test_notFoundForNonSuspendedUser cfg = do
  targetInsert <- mkUserInsert "usr-unsuspend-target-notsuspended" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    -- Target is not suspended, so unsuspend should fail to find the row
    result <- runExceptT $ action targetId

    liftIO $ case result of
      Right (TargetUserNotFound _) -> pure ()
      Right (UnsuspendSuccess _) -> expectationFailure "Expected TargetUserNotFound but got UnsuspendSuccess"
      Right (UnsuspendFailed _) -> expectationFailure "Expected TargetUserNotFound but got UnsuspendFailed"
      Left err -> expectationFailure $ "Expected Right TargetUserNotFound but got Left: " <> show err

-- | Unsuspends a previously suspended user and returns UnsuspendSuccess.
test_unsuspendsSuspendedUser :: TestDBConfig -> IO ()
test_unsuspendsSuspendedUser cfg = do
  targetInsert <- mkUserInsert "usr-unsuspend-target-success" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      targetId <- insertTestUser targetInsert
      -- Suspend the target user first so we can then unsuspend them
      _ <- TRX.statement () (UserMetadata.suspendUser targetId "setup suspension")
      pure targetId
    targetId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action targetId

    liftIO $ case result of
      Right (UnsuspendSuccess _) -> pure ()
      Right (TargetUserNotFound _) -> expectationFailure "Expected UnsuspendSuccess but got TargetUserNotFound"
      Right (UnsuspendFailed err) -> expectationFailure $ "Expected UnsuspendSuccess but got UnsuspendFailed: " <> show err
      Left err -> expectationFailure $ "Expected Right UnsuspendSuccess but got Left: " <> show err

-- | UnsuspendSuccess carries the correct target user ID in the UserWithMetadata.
test_successCarriesCorrectId :: TestDBConfig -> IO ()
test_successCarriesCorrectId cfg = do
  targetInsert <- mkUserInsert "usr-unsuspend-target-correctid" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      targetId <- insertTestUser targetInsert
      -- Suspend the target user first
      _ <- TRX.statement () (UserMetadata.suspendUser targetId "setup for unsuspend test")
      pure targetId
    targetId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action targetId

    liftIO $ case result of
      Right (UnsuspendSuccess updatedUser) ->
        updatedUser.uwmUserId `shouldBe` targetId
      Right (TargetUserNotFound _) -> expectationFailure "Expected UnsuspendSuccess but got TargetUserNotFound"
      Right (UnsuspendFailed err) -> expectationFailure $ "Expected UnsuspendSuccess but got UnsuspendFailed: " <> show err
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
