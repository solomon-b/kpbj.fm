module API.User.Login.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (rootGetLink, userLoginGetLink)
import App.Auth qualified as Auth
import App.Errors (InternalServerError (..), throwErr)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.Password.Argon2 (Password, PasswordCheck (..), checkPassword, mkPassword)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..), RecordInstance (..))
import Deriving.Aeson qualified as Deriving
import Domain.Types.EmailAddress
import Effects.Clock (MonadClock)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.ServerSessions qualified as Session
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Hasql.Pool qualified
import Log qualified
import Network.Socket
import OpenTelemetry.Trace qualified as OTEL
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as FormUrlEncoded
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

type Route =
  "user"
    :> "login"
    :> Servant.RemoteHost
    :> Servant.Header "User-Agent" Text
    :> Servant.ReqBody '[Servant.FormUrlEncoded] Login
    :> Servant.QueryParam "redirect" Text
    :> Servant.PostAccepted '[HTML] (Servant.Headers '[Servant.Header "Set-Cookie" Text, Servant.Header "HX-Redirect" Text] Servant.NoContent)

--------------------------------------------------------------------------------

data Login = Login
  { ulEmail :: EmailAddress,
    ulPassword :: Password
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance Login)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ul", Deriving.CamelToSnake]] Login

instance FormUrlEncoded.FromForm Login where
  fromForm f =
    Login
      <$> FormUrlEncoded.parseUnique "email" f
      <*> fmap mkPassword (FormUrlEncoded.parseUnique "password" f)

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
handler sockAddr mUserAgent Login {..} redirectQueryParam = do
  Observability.handlerSpan "POST /user/login" $ do
    execQuerySpanThrow (User.getUserByEmail ulEmail) >>= \case
      Just user -> do
        Log.logInfo "Login Attempt" ulEmail
        if checkPassword ulPassword (User.mPassword user) == PasswordCheckSuccess
          then
            let redirectLink = fromMaybe (Http.toUrlPiece rootGetLink) redirectQueryParam
             in attemptLogin sockAddr mUserAgent redirectLink user
          else invalidCredentialResponse ulEmail (Aeson.object [("field", "password"), "value" .= ulPassword])
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
  pure $ Servant.noHeader $ Servant.addHeader ("/" <> Http.toUrlPiece (userLoginGetLink Nothing $ Just emailAddress)) Servant.NoContent
