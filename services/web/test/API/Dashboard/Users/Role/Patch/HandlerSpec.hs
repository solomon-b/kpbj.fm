module API.Dashboard.Users.Role.Patch.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Role.Patch.Handler (RoleUpdateResult (..), action)
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
    describe "API.Dashboard.Users.Role.Patch.Handler" $ do
      describe "action" $ do
        it "returns RoleUpdateNotFound for missing user" test_notFoundForMissingUser
        it "updates role successfully" test_updatesRoleSuccessfully
        it "RoleUpdateSuccess carries the new role" test_successCarriesNewRole
        it "can update role to Admin" test_canUpdateToAdmin

--------------------------------------------------------------------------------

-- | Render a RoleUpdateResult to a string for test failure messages.
describeResult :: RoleUpdateResult -> String
describeResult RoleUpdateNotFound = "RoleUpdateNotFound"
describeResult RoleUpdateFailed = "RoleUpdateFailed"
describeResult (RoleUpdateSuccess _) = "RoleUpdateSuccess"

--------------------------------------------------------------------------------

-- | Returns RoleUpdateNotFound for a user ID that does not exist.
test_notFoundForMissingUser :: TestDBConfig -> IO ()
test_notFoundForMissingUser cfg = do
  bracketAppM cfg $ do
    result <- runExceptT $ action (User.Id nonExistentId) UserMetadata.Host
    liftIO $ case result of
      Right RoleUpdateNotFound -> pure ()
      Right other -> expectationFailure $ "Expected RoleUpdateNotFound but got: " <> describeResult other
      Left err -> expectationFailure $ "Expected Right RoleUpdateNotFound but got Left: " <> show err

-- | Updates a user's role from User to Host successfully.
test_updatesRoleSuccessfully :: TestDBConfig -> IO ()
test_updatesRoleSuccessfully cfg = do
  targetInsert <- mkUserInsert "usr-role-target-update" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action targetId UserMetadata.Host

    liftIO $ case result of
      Right (RoleUpdateSuccess _) -> pure ()
      Right other -> expectationFailure $ "Expected RoleUpdateSuccess but got: " <> describeResult other
      Left err -> expectationFailure $ "Expected Right RoleUpdateSuccess but got Left: " <> show err

-- | The updated role in RoleUpdateSuccess matches the role we requested.
test_successCarriesNewRole :: TestDBConfig -> IO ()
test_successCarriesNewRole cfg = do
  targetInsert <- mkUserInsert "usr-role-target-carries" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action targetId UserMetadata.Staff

    liftIO $ case result of
      Right (RoleUpdateSuccess newRole) -> newRole `shouldBe` UserMetadata.Staff
      Right other -> expectationFailure $ "Expected RoleUpdateSuccess Staff but got: " <> describeResult other
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err

-- | Can update a user's role to Admin.
test_canUpdateToAdmin :: TestDBConfig -> IO ()
test_canUpdateToAdmin cfg = do
  targetInsert <- mkUserInsert "usr-role-target-admin" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action targetId UserMetadata.Admin

    liftIO $ case result of
      Right (RoleUpdateSuccess newRole) -> newRole `shouldBe` UserMetadata.Admin
      Right other -> expectationFailure $ "Expected RoleUpdateSuccess Admin but got: " <> describeResult other
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
