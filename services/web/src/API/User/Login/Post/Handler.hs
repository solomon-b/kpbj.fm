{-# LANGUAGE QuasiQuotes #-}

module API.User.Login.Post.Handler where

--------------------------------------------------------------------------------

import API.Links (apiLinks, userLinks)
import API.Types (Routes (..), UserRoutes (..))
import API.User.Login.Post.Route (Login (..))
import App.Auth qualified as Auth
import App.Config (Environment)
import App.Domains qualified as Domains
import App.Handler.Error (HandlerError, logHandlerError, throwHandlerFailure)
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has qualified as Has
import Data.Password.Argon2 (PasswordCheck (..), checkPassword)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.EmailAddress ()
import Effects.Database.Execute (execQuery, execQueryThrow)
import Effects.Database.Tables.EmailVerificationTokens qualified as VerificationTokens
import Effects.Database.Tables.ServerSessions qualified as ServerSessions
import Effects.Database.Tables.User qualified as User
import Hasql.Interpolate (interp, sql)
import Hasql.Statement qualified as Hasql
import Log qualified
import Network.Socket
import Servant qualified
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

-- | Result type for a login attempt.
data LoginResult
  = -- | Successful login: session cookie, redirect URL.
    LoginSuccess Text Text
  | -- | Email not verified — redirect URL to verification-sent page.
    LoginEmailNotVerified Text
  | -- | Invalid credentials — redirect URL back to login page.
    LoginInvalidCredentials Text

--------------------------------------------------------------------------------

-- | Core login business logic.
--
-- Validates credentials, checks email verification, and either creates a new
-- session or reuses an existing one. Returns a 'LoginResult' describing how
-- the handler should respond.
action ::
  SockAddr ->
  Maybe Text ->
  Login ->
  Maybe Text ->
  ExceptT HandlerError AppM LoginResult
action sockAddr mUserAgent Login {..} redirectQueryParam = do
  mUser <- execQueryThrow (User.getUserByEmail ulEmail)
  case mUser of
    Just user
      | checkPassword ulPassword (User.mPassword user) == PasswordCheckSuccess -> do
          Log.logInfo "Login Attempt" ulEmail
          isVerifiedResult <- execQuery (VerificationTokens.isUserEmailVerified (User.mId user))
          case isVerifiedResult of
            Right (Just _) -> do
              let isRelative url = Text.isPrefixOf "/" url && not (Text.isPrefixOf "//" url)
              let redirectLink = case redirectQueryParam of
                    Just url | isRelative url -> url
                    _ -> "/" <> Http.toUrlPiece apiLinks.rootGet
              buildLoginSuccess sockAddr mUserAgent redirectLink user ulRememberMe
            _ -> do
              Log.logInfo "Login blocked - email not verified" ulEmail
              pure $ LoginEmailNotVerified ("/" <> Http.toUrlPiece (userLinks.verifyEmailSentGet $ Just ulEmail))
    Just _user -> do
      Log.logInfo "Login Attempt" ulEmail
      Log.logInfo "Invalid Credentials" (Aeson.object ["field" .= ("password" :: Text)])
      pure $ LoginInvalidCredentials ("/" <> Http.toUrlPiece (userLinks.loginGet Nothing $ Just ulEmail))
    Nothing -> do
      Log.logInfo "Invalid Credentials" (Aeson.object [("field", "email"), "value" .= ulEmail])
      pure $ LoginInvalidCredentials ("/" <> Http.toUrlPiece (userLinks.loginGet Nothing $ Just ulEmail))

-- | Build session cookie and redirect URL for a successful login.
--
-- When @rememberMe@ is 'True', extends the server-side session expiry to
-- 'rememberMeDays' (overriding the upstream 1-day default) and emits a
-- persistent cookie with @Max-Age@ instead of a browser-session cookie.
buildLoginSuccess ::
  SockAddr ->
  Maybe Text ->
  Text ->
  User.Model ->
  Bool ->
  ExceptT HandlerError AppM LoginResult
buildLoginSuccess sockAddr mUserAgent redirectLink user rememberMe = do
  env <- asks (Has.getter @Environment)
  -- Always create a fresh session (upstream insertServerSession caps at 5 per user)
  sessionId <-
    lift (Auth.login (User.mId user) sockAddr mUserAgent) >>= \case
      Left err -> throwHandlerFailure $ Text.pack $ show err
      Right sid -> pure sid
  cookieHeader <-
    if rememberMe
      then do
        lift $ execQueryThrow (extendSessionExpiry sessionId)
        pure $ mkPersistentCookieSession env (Domains.cookieDomainMaybe env) sessionId rememberMeMaxAgeSeconds
      else pure $ Auth.mkCookieSession env (Domains.cookieDomainMaybe env) sessionId
  pure $ LoginSuccess cookieHeader redirectLink

-- | Cookie @Max-Age@ when "Remember Me" is checked: 30 days in seconds.
rememberMeMaxAgeSeconds :: Integer
rememberMeMaxAgeSeconds = 30 * 24 * 60 * 60

-- | Extend a session row's @expires_at@ to 30 days from now. Used for
-- "Remember Me" because upstream 'Auth.login' hardcodes a 1-day expiry.
extendSessionExpiry :: ServerSessions.Id -> Hasql.Statement () ()
extendSessionExpiry sid =
  interp
    False
    [sql|
    UPDATE server_sessions
    SET expires_at = NOW() + INTERVAL '30 days'
    WHERE id = #{sid}
  |]

-- | Like 'Auth.mkCookieSession' but with a @Max-Age@ attribute appended,
-- producing a persistent cookie that survives browser restarts.
mkPersistentCookieSession :: Environment -> Maybe Text -> ServerSessions.Id -> Integer -> Text
mkPersistentCookieSession env mDomain sId maxAge =
  Auth.mkCookieSession env mDomain sId <> "; Max-Age=" <> Text.pack (show maxAge)

handler ::
  SockAddr ->
  Maybe Text ->
  Login ->
  Maybe Text ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler sockAddr mUserAgent loginForm redirectQueryParam = do
  result <- runExceptT $ action sockAddr mUserAgent loginForm redirectQueryParam
  case result of
    Left err -> do
      logHandlerError "Login" err
      pure $
        Servant.noHeader $
          Servant.addHeader ("/" <> Http.toUrlPiece (userLinks.loginGet Nothing Nothing)) Servant.NoContent
    Right (LoginSuccess cookieHeader redirectLink) ->
      pure $
        Servant.addHeader cookieHeader $
          Servant.addHeader redirectLink Servant.NoContent
    Right (LoginEmailNotVerified redirectUrl) ->
      pure $ Servant.noHeader $ Servant.addHeader redirectUrl Servant.NoContent
    Right (LoginInvalidCredentials redirectUrl) ->
      pure $ Servant.noHeader $ Servant.addHeader redirectUrl Servant.NoContent
