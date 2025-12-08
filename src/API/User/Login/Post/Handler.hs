module API.User.Login.Post.Handler where

--------------------------------------------------------------------------------

import API.Links (apiLinks, userLinks)
import API.Types
import API.User.Login.Post.Route (Login (..))
import App.Auth qualified as Auth
import App.Errors (InternalServerError (..), throwErr)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.Password.Argon2 (PasswordCheck (..), checkPassword)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.EmailAddress
import Effects.Clock (MonadClock)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.ServerSessions qualified as Session
import Effects.Database.Tables.User qualified as User
import Hasql.Pool qualified
import Log qualified
import Network.Socket
import OpenTelemetry.Trace qualified as OTEL
import Servant qualified
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

handler ::
  ( MonadClock m,
    MonadReader env m,
    Has Hasql.Pool.Pool env,
    MonadIO m,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m,
    Has OTEL.Tracer env
  ) =>
  OTEL.Tracer ->
  SockAddr ->
  Maybe Text ->
  Login ->
  Maybe Text ->
  m
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
          let redirectLink = fromMaybe (Http.toUrlPiece apiLinks.rootGet) redirectQueryParam
          attemptLogin sockAddr mUserAgent redirectLink user
    Just _user -> do
      Log.logInfo "Login Attempt" ulEmail
      invalidCredentialResponse ulEmail (Aeson.object [("field", "password"), "value" .= ulPassword])
    Nothing ->
      invalidCredentialResponse ulEmail (Aeson.object [("field", "email"), "value" .= ulEmail])

attemptLogin ::
  ( MonadClock m,
    MonadUnliftIO m,
    Has OTEL.Tracer env,
    Has Hasql.Pool.Pool env,
    MonadReader env m,
    MonadThrow m,
    MonadDB m,
    Log.MonadLog m
  ) =>
  SockAddr ->
  Maybe Text ->
  Text ->
  User.Model ->
  m
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
  ( Log.MonadLog m,
    ToJSON details
  ) =>
  EmailAddress ->
  details ->
  m
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
invalidCredentialResponse emailAddress details = do
  Log.logInfo "Invalid Credentials" details
  pure $ Servant.noHeader $ Servant.addHeader ("/" <> Http.toUrlPiece (userLinks.loginGet Nothing $ Just emailAddress)) Servant.NoContent
