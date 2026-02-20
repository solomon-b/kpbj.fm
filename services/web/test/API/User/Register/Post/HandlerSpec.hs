module API.User.Register.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.User.Register.Post.Handler (RegisterResult (..), action)
import API.User.Register.Post.Route (Register (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Password.Argon2 (mkPassword)
import Domain.Types.DisplayName (mkDisplayNameUnsafe)
import Domain.Types.EmailAddress (mkEmailAddress)
import Domain.Types.FullName (mkFullNameUnsafe)
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
    describe "API.User.Register.Post.Handler" $ do
      describe "action" $ do
        it "returns RegisterFailure for validation errors" test_registrationValidationFailure
        it "returns RegisterFailure for duplicate email" test_registrationDuplicateEmail
        it "registers a new user successfully" test_registersNewUser

--------------------------------------------------------------------------------

testSockAddr :: SockAddr
testSockAddr = SockAddrUnix "test"

-- | Display a RegisterResult for test failure messages.
showRegisterResult :: RegisterResult -> String
showRegisterResult (RegisterSuccess _) = "RegisterSuccess"
showRegisterResult (RegisterFailure _) = "RegisterFailure"

-- | Build a valid Register form.
validRegisterForm :: Register
validRegisterForm =
  Register
    { urEmail = mkEmailAddress "new-user@test.example.com",
      urPassword = mkPassword "ValidPassword123!",
      urDisplayName = mkDisplayNameUnsafe "New User",
      urFullName = mkFullNameUnsafe "New User Full Name",
      urNewsletter = Nothing
    }

--------------------------------------------------------------------------------

-- | Submitting a register form with an invalid password returns RegisterFailure.
test_registrationValidationFailure :: TestDBConfig -> IO ()
test_registrationValidationFailure cfg = do
  let form = validRegisterForm {urPassword = mkPassword "short"}
  bracketAppM cfg $ do
    result <- runExceptT $ action testSockAddr Nothing form
    liftIO $ case result of
      Right (RegisterFailure _) -> pure ()
      Right other -> expectationFailure $ "Expected RegisterFailure but got: " <> showRegisterResult other
      Left err -> expectationFailure $ "Expected Right RegisterFailure but got Left: " <> show err

-- | Registering with an already-used email returns RegisterFailure.
test_registrationDuplicateEmail :: TestDBConfig -> IO ()
test_registrationDuplicateEmail cfg = do
  userInsert <- mkUserInsert "register-dup" UserMetadata.User
  bracketAppM cfg $ do
    -- Pre-insert a user with the same email
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser userInsert
    _ <- liftIO $ expectSetupRight dbResult

    let form = validRegisterForm {urEmail = mkEmailAddress "register-dup@test.example.com"}
    result <- runExceptT $ action testSockAddr Nothing form
    liftIO $ case result of
      Right (RegisterFailure _) -> pure ()
      Right other -> expectationFailure $ "Expected RegisterFailure but got: " <> showRegisterResult other
      Left err -> expectationFailure $ "Expected Right RegisterFailure but got Left: " <> show err

-- | Successful registration creates user and returns RegisterSuccess.
test_registersNewUser :: TestDBConfig -> IO ()
test_registersNewUser cfg = do
  bracketAppM cfg $ do
    let form =
          validRegisterForm
            { urEmail = mkEmailAddress "register-success@test.example.com",
              urPassword = mkPassword "ValidPassword123!"
            }
    result <- runExceptT $ action testSockAddr Nothing form
    liftIO $ case result of
      Right (RegisterSuccess _) -> pure ()
      Right other -> expectationFailure $ "Expected RegisterSuccess but got: " <> showRegisterResult other
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
