module API.User.Login.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.User.Login.Post.Handler (LoginResult (..), action)
import API.User.Login.Post.Route (Login (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Password.Argon2 (mkPassword)
import Domain.Types.EmailAddress (mkEmailAddress)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Network.Socket (SockAddr (..))
import Test.Database.Helpers (insertTestUser, verifyTestUserEmail)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.User.Login.Post.Handler" $ do
      describe "action" $ do
        it "returns LoginSuccess for valid credentials with verified email" test_successfulLogin
        it "returns LoginInvalidCredentials for non-existent email" test_invalidCredentialsNoUser
        it "returns LoginInvalidCredentials for wrong password" test_invalidCredentialsWrongPassword
        it "returns LoginEmailNotVerified for unverified user" test_emailNotVerified

--------------------------------------------------------------------------------

testSockAddr :: SockAddr
testSockAddr = SockAddrUnix "test"

-- | Display a LoginResult for test failure messages.
showLoginResult :: LoginResult -> String
showLoginResult (LoginSuccess {}) = "LoginSuccess"
showLoginResult (LoginEmailNotVerified _) = "LoginEmailNotVerified"
showLoginResult (LoginInvalidCredentials _) = "LoginInvalidCredentials"

--------------------------------------------------------------------------------

-- | Logging in with correct password and verified email returns LoginSuccess.
test_successfulLogin :: TestDBConfig -> IO ()
test_successfulLogin cfg = do
  userInsert <- mkUserInsert "login-success" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          userId <- insertTestUser userInsert
          verifyTestUserEmail userId
    _ <- liftIO $ expectSetupRight dbResult

    let loginForm = Login (mkEmailAddress "login-success@test.example.com") (mkPassword "testpassword")
    result <- runExceptT $ action testSockAddr Nothing loginForm Nothing
    liftIO $ case result of
      Right (LoginSuccess {}) -> pure ()
      Right other -> expectationFailure $ "Expected LoginSuccess but got: " <> showLoginResult other
      Left err -> expectationFailure $ "Expected Right LoginSuccess but got Left: " <> show err

-- | Attempting to login with a non-existent email returns invalid credentials.
test_invalidCredentialsNoUser :: TestDBConfig -> IO ()
test_invalidCredentialsNoUser cfg = do
  let loginForm = Login (mkEmailAddress "nobody@test.example.com") (mkPassword "somepassword")
  bracketAppM cfg $ do
    result <- runExceptT $ action testSockAddr Nothing loginForm Nothing
    liftIO $ case result of
      Right (LoginInvalidCredentials _) -> pure ()
      Right other -> expectationFailure $ "Expected LoginInvalidCredentials but got: " <> showLoginResult other
      Left err -> expectationFailure $ "Expected Right LoginInvalidCredentials but got Left: " <> show err

-- | Attempting to login with wrong password returns invalid credentials.
test_invalidCredentialsWrongPassword :: TestDBConfig -> IO ()
test_invalidCredentialsWrongPassword cfg = do
  userInsert <- mkUserInsert "login-wrong-pass" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser userInsert
    _ <- liftIO $ expectSetupRight dbResult

    let loginForm = Login (mkEmailAddress "login-wrong-pass@test.example.com") (mkPassword "wrongpassword")
    result <- runExceptT $ action testSockAddr Nothing loginForm Nothing
    liftIO $ case result of
      Right (LoginInvalidCredentials _) -> pure ()
      Right other -> expectationFailure $ "Expected LoginInvalidCredentials but got: " <> showLoginResult other
      Left err -> expectationFailure $ "Expected Right LoginInvalidCredentials but got Left: " <> show err

-- | Logging in with correct password but unverified email returns LoginEmailNotVerified.
test_emailNotVerified :: TestDBConfig -> IO ()
test_emailNotVerified cfg = do
  userInsert <- mkUserInsert "login-unverified" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser userInsert
    _ <- liftIO $ expectSetupRight dbResult

    -- "testpassword" is the password set by mkUserInsert
    let loginForm = Login (mkEmailAddress "login-unverified@test.example.com") (mkPassword "testpassword")
    result <- runExceptT $ action testSockAddr Nothing loginForm Nothing
    liftIO $ case result of
      Right (LoginEmailNotVerified _) -> pure ()
      Right other -> expectationFailure $ "Expected LoginEmailNotVerified but got: " <> showLoginResult other
      Left err -> expectationFailure $ "Expected Right LoginEmailNotVerified but got Left: " <> show err
