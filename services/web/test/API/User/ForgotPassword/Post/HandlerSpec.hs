module API.User.ForgotPassword.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.User.ForgotPassword.Post.Handler (action)
import API.User.ForgotPassword.Post.Route (ForgotPasswordForm (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Network.Socket (SockAddr (..))
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.User.ForgotPassword.Post.Handler" $ do
      describe "action" $ do
        it "succeeds for non-existent email (prevents enumeration)" test_succeedsForNonExistentEmail
        it "succeeds for existing user (creates reset token)" test_succeedsForExistingUser

--------------------------------------------------------------------------------

testSockAddr :: SockAddr
testSockAddr = SockAddrUnix "test"

--------------------------------------------------------------------------------

-- | Requesting a password reset for a non-existent email still succeeds
-- (prevents email enumeration).
test_succeedsForNonExistentEmail :: TestDBConfig -> IO ()
test_succeedsForNonExistentEmail cfg = do
  let form = ForgotPasswordForm "nonexistent@test.example.com"
  bracketAppM cfg $ do
    result <- runExceptT $ action testSockAddr Nothing form
    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected Right () but got Left: " <> show err

-- | Requesting a password reset for an existing user returns Right.
test_succeedsForExistingUser :: TestDBConfig -> IO ()
test_succeedsForExistingUser cfg = do
  userInsert <- mkUserInsert "forgot-pw-user" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser userInsert
    _ <- liftIO $ expectSetupRight dbResult

    let form = ForgotPasswordForm "forgot-pw-user@test.example.com"
    result <- runExceptT $ action testSockAddr Nothing form
    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected Right () but got Left: " <> show err
