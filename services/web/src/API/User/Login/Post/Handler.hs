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
import Data.Time (NominalDiffTime, nominalDay)
import Domain.Types.EmailAddress ()
import Effects.Database.Execute (execQuery, execQueryThrow)
import Effects.Database.Tables.EmailVerificationTokens qualified as VerificationTokens
import Effects.Database.Tables.User qualified as User
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
-- When @rememberMe@ is 'True', creates a 30-day session and emits a
-- persistent cookie with a matching @Max-Age@. Otherwise the upstream
-- 1-day default applies and the cookie is a browser-session cookie.
buildLoginSuccess ::
  SockAddr ->
  Maybe Text ->
  Text ->
  User.Model ->
  Bool ->
  ExceptT HandlerError AppM LoginResult
buildLoginSuccess sockAddr mUserAgent redirectLink user rememberMe = do
  env <- asks (Has.getter @Environment)
  let duration = if rememberMe then rememberMeDuration else nominalDay
  -- Always create a fresh session (upstream insertServerSession caps at 5 per user)
  result <-
    lift (Auth.loginWithExpiry duration (User.mId user) sockAddr mUserAgent) >>= \case
      Left err -> throwHandlerFailure $ Text.pack $ show err
      Right r -> pure r
  let sessionId = Auth.loginResultSessionId result
      cookieHeader =
        if rememberMe
          then Auth.mkCookieSessionWithMaxAge (Auth.loginResultDuration result) env (Domains.cookieDomainMaybe env) sessionId
          else Auth.mkCookieSession env (Domains.cookieDomainMaybe env) sessionId
  pure $ LoginSuccess cookieHeader redirectLink

-- | Server-side session lifetime when "Remember Me" is checked.
rememberMeDuration :: NominalDiffTime
rememberMeDuration = 30 * nominalDay

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
