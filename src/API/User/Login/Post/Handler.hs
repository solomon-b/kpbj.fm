module API.User.Login.Post.Handler where

--------------------------------------------------------------------------------

import API.Links (apiLinks, userLinks)
import API.Types
import API.User.Login.Post.Route (Login (..))
import App.Auth qualified as Auth
import App.Errors (InternalServerError (..), throwErr)
import App.Monad (AppM)
import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe)
import Data.Password.Argon2 (PasswordCheck (..), checkPassword)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.EmailAddress
import Effects.Database.Execute (execQuerySpan, execQuerySpanThrow)
import Effects.Database.Tables.EmailVerificationTokens qualified as VerificationTokens
import Effects.Database.Tables.ServerSessions qualified as Session
import Effects.Database.Tables.User qualified as User
import Log qualified
import Network.Socket
import OpenTelemetry.Trace qualified as OTEL
import Servant qualified
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

handler ::
  OTEL.Tracer ->
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
handler _tracer sockAddr mUserAgent Login {..} redirectQueryParam = do
  execQuerySpanThrow (User.getUserByEmail ulEmail) >>= \case
    Just user
      | checkPassword ulPassword (User.mPassword user) == PasswordCheckSuccess -> do
          Log.logInfo "Login Attempt" ulEmail
          -- Check if email is verified
          isVerifiedResult <- execQuerySpan (VerificationTokens.isUserEmailVerified (User.mId user))
          case isVerifiedResult of
            Right (Just _) -> do
              -- Email is verified, proceed with login
              let redirectLink = fromMaybe (Http.toUrlPiece apiLinks.rootGet) redirectQueryParam
              attemptLogin sockAddr mUserAgent redirectLink user
            _ -> do
              -- Email not verified or query failed - redirect to verify-email/sent page
              Log.logInfo "Login blocked - email not verified" ulEmail
              emailNotVerifiedResponse ulEmail
    Just _user -> do
      Log.logInfo "Login Attempt" ulEmail
      invalidCredentialResponse ulEmail (Aeson.object [("field", "password"), "value" .= ulPassword])
    Nothing ->
      invalidCredentialResponse ulEmail (Aeson.object [("field", "email"), "value" .= ulEmail])

attemptLogin ::
  SockAddr ->
  Maybe Text ->
  Text ->
  User.Model ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
attemptLogin sockAddr mUserAgent redirectLink user = do
  execQuerySpanThrow (Session.getServerSessionByUser (User.mId user)) >>= \case
    Nothing -> do
      Auth.login (User.mId user) sockAddr mUserAgent >>= \case
        Left err ->
          throwErr $ InternalServerError $ Text.pack $ show err
        Right sessionId -> do
          pure $ Servant.addHeader (Auth.mkCookieSession sessionId) $ Servant.addHeader redirectLink Servant.NoContent
    Just session ->
      let sessionId = Session.mSessionId session
       in pure $ Servant.addHeader (Auth.mkCookieSession sessionId) $ Servant.addHeader redirectLink Servant.NoContent

invalidCredentialResponse ::
  (ToJSON details) =>
  EmailAddress ->
  details ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
invalidCredentialResponse emailAddress details = do
  Log.logInfo "Invalid Credentials" details
  pure $ Servant.noHeader $ Servant.addHeader ("/" <> Http.toUrlPiece (userLinks.loginGet Nothing $ Just emailAddress)) Servant.NoContent

emailNotVerifiedResponse ::
  EmailAddress ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
emailNotVerifiedResponse emailAddress = do
  Log.logInfo "Email not verified" emailAddress
  pure $ Servant.noHeader $ Servant.addHeader ("/" <> Http.toUrlPiece (userLinks.verifyEmailSentGet $ Just emailAddress)) Servant.NoContent
