{-# LANGUAGE QuasiQuotes #-}

module API.User.Login.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.User.Login.Post.Handler (LoginResult (..), action)
import API.User.Login.Post.Route (Login (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Password.Argon2 (mkPassword)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Domain.Types.EmailAddress (mkEmailAddress)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Interpolate (getOneColumn, interp, sql)
import Hasql.Statement qualified as Hasql
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
        it "with remember-me sets Max-Age cookie and 30-day session expiry" test_rememberMePersistsSession
        it "without remember-me uses a browser-session cookie" test_noRememberMeBrowserSessionCookie

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

    let loginForm = Login (mkEmailAddress "login-success@test.example.com") (mkPassword "testpassword") False
    result <- runExceptT $ action testSockAddr Nothing loginForm Nothing
    liftIO $ case result of
      Right (LoginSuccess {}) -> pure ()
      Right other -> expectationFailure $ "Expected LoginSuccess but got: " <> showLoginResult other
      Left err -> expectationFailure $ "Expected Right LoginSuccess but got Left: " <> show err

-- | Attempting to login with a non-existent email returns invalid credentials.
test_invalidCredentialsNoUser :: TestDBConfig -> IO ()
test_invalidCredentialsNoUser cfg = do
  let loginForm = Login (mkEmailAddress "nobody@test.example.com") (mkPassword "somepassword") False
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

    let loginForm = Login (mkEmailAddress "login-wrong-pass@test.example.com") (mkPassword "wrongpassword") False
    result <- runExceptT $ action testSockAddr Nothing loginForm Nothing
    liftIO $ case result of
      Right (LoginInvalidCredentials _) -> pure ()
      Right other -> expectationFailure $ "Expected LoginInvalidCredentials but got: " <> showLoginResult other
      Left err -> expectationFailure $ "Expected Right LoginInvalidCredentials but got Left: " <> show err

-- | With @remember = true@, login emits a persistent @Max-Age@ cookie and the
-- server-side session row's @expires_at@ is extended to ~30 days from now
-- (overriding upstream @Auth.login@'s 1-day default).
test_rememberMePersistsSession :: TestDBConfig -> IO ()
test_rememberMePersistsSession cfg = do
  userInsert <- mkUserInsert "login-remember-me" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          userId <- insertTestUser userInsert
          verifyTestUserEmail userId
          pure userId
    userId <- liftIO $ expectSetupRight dbResult

    let loginForm = Login (mkEmailAddress "login-remember-me@test.example.com") (mkPassword "testpassword") True
    result <- runExceptT $ action testSockAddr Nothing loginForm Nothing
    case result of
      Right (LoginSuccess cookie _) -> do
        liftIO $
          if "Max-Age=2592000" `Text.isInfixOf` cookie
            then pure ()
            else expectationFailure $ "Expected cookie to contain Max-Age=2592000, got: " <> Text.unpack cookie
        mExpiresAt <- execQueryThrow (latestSessionExpiry userId)
        liftIO $ assertExpiryAround mExpiresAt thirtyDays
      Right other -> liftIO $ expectationFailure $ "Expected LoginSuccess but got: " <> showLoginResult other
      Left err -> liftIO $ expectationFailure $ "Expected Right LoginSuccess but got Left: " <> show err

-- | With @remember = false@ (the default), login emits a browser-session
-- cookie with no @Max-Age@ attribute.
test_noRememberMeBrowserSessionCookie :: TestDBConfig -> IO ()
test_noRememberMeBrowserSessionCookie cfg = do
  userInsert <- mkUserInsert "login-no-remember-me" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          userId <- insertTestUser userInsert
          verifyTestUserEmail userId
    _ <- liftIO $ expectSetupRight dbResult

    let loginForm = Login (mkEmailAddress "login-no-remember-me@test.example.com") (mkPassword "testpassword") False
    result <- runExceptT $ action testSockAddr Nothing loginForm Nothing
    liftIO $ case result of
      Right (LoginSuccess cookie _) ->
        if "Max-Age" `Text.isInfixOf` cookie
          then expectationFailure $ "Expected cookie without Max-Age, got: " <> Text.unpack cookie
          else pure ()
      Right other -> expectationFailure $ "Expected LoginSuccess but got: " <> showLoginResult other
      Left err -> expectationFailure $ "Expected Right LoginSuccess but got Left: " <> show err

-- | Tolerance window when checking that @expires_at@ is roughly N seconds in
-- the future. Allows for clock drift between the test process and Postgres,
-- plus the time spent inside the handler between session creation and the
-- test's reference timestamp.
expirySlackSeconds :: NominalDiffTime
expirySlackSeconds = 60

thirtyDays :: NominalDiffTime
thirtyDays = 30 * 24 * 60 * 60

-- | Assert the given @expires_at@ is within 'expirySlackSeconds' of @target@
-- seconds from now. Fails the test otherwise.
assertExpiryAround :: Maybe UTCTime -> NominalDiffTime -> IO ()
assertExpiryAround Nothing _ = expectationFailure "Expected a server_sessions row but found none"
assertExpiryAround (Just expiresAt) target = do
  now <- getCurrentTime
  let delta = diffUTCTime expiresAt now
      diff = abs (delta - target)
  if diff <= expirySlackSeconds
    then pure ()
    else
      expectationFailure $
        "Expected session to expire ~"
          <> show target
          <> " from now, but got delta "
          <> show delta

-- | Most recent @expires_at@ for any session belonging to a user.
latestSessionExpiry :: User.Id -> Hasql.Statement () (Maybe UTCTime)
latestSessionExpiry uid =
  fmap (fmap getOneColumn) $
    interp
      False
      [sql|
    SELECT expires_at
    FROM server_sessions
    WHERE user_id = #{uid}
    ORDER BY expires_at DESC
    LIMIT 1
  |]

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
    let loginForm = Login (mkEmailAddress "login-unverified@test.example.com") (mkPassword "testpassword") False
    result <- runExceptT $ action testSockAddr Nothing loginForm Nothing
    liftIO $ case result of
      Right (LoginEmailNotVerified _) -> pure ()
      Right other -> expectationFailure $ "Expected LoginEmailNotVerified but got: " <> showLoginResult other
      Left err -> expectationFailure $ "Expected Right LoginEmailNotVerified but got Left: " <> show err
