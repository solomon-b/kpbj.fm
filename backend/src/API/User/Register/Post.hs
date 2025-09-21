module API.User.Register.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (userRegisterGetLink)
import App.Auth qualified as Auth
import App.Errors (Forbidden (..), InternalServerError (..), throwErr)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Catch.Pure (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Has (Has)
import Data.Password.Argon2 (Argon2, Password, PasswordHash, hashPassword, mkPassword)
import Data.Password.Validate qualified as PW.Validate
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, display)
import Data.Text.Display.Core (Display (..))
import Data.Text.Display.Generic (RecordInstance (..))
import Data.Validation
import Deriving.Aeson qualified as Deriving
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.EmailAddress qualified as EmailAddress
import Domain.Types.FullName (FullName)
import Effects.Clock (MonadClock)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Hasql.Interpolate (OneRow (..))
import Hasql.Pool qualified
import Log qualified
import Network.Socket (SockAddr)
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances.OneRow ()
import OrphanInstances.Servant ()
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded qualified as FormUrlEncoded
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

type Route =
  "user"
    :> "register"
    :> Servant.RemoteHost
    :> Servant.Header "User-Agent" Text
    :> Servant.ReqBody '[Servant.FormUrlEncoded] Register
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "Set-Cookie" Text, Servant.Header "HX-Redirect" Text] Servant.NoContent)

--------------------------------------------------------------------------------

data Register = Register
  { urEmail :: EmailAddress,
    urPassword :: Password,
    urDisplayName :: DisplayName,
    urFullName :: FullName
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance Register)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ur", Deriving.CamelToSnake]] Register

instance FormUrlEncoded.FromForm Register where
  fromForm f =
    Register
      <$> FormUrlEncoded.parseUnique "email" f
      <*> fmap mkPassword (FormUrlEncoded.parseUnique "password" f)
      <*> FormUrlEncoded.parseUnique "displayName" f
      <*> FormUrlEncoded.parseUnique "fullName" f

data RegisterParsed = RegisterParsed
  { urpEmail :: EmailAddress,
    urpPassword :: PasswordHash Argon2,
    urpDisplayName :: DisplayName,
    urpFullName :: FullName
  }

--------------------------------------------------------------------------------

data ValidationError
  = InvalidEmailAddress
  | InvalidPassword [PW.Validate.InvalidReason]
  | EmptyDisplayName
  | EmptyFullName

instance Display ValidationError where
  displayBuilder = \case
    InvalidEmailAddress -> "Email address is invalid."
    InvalidPassword reasons -> "Password is invalid: " <> foldl' (\acc x -> acc <> ", " <> displayBuilder x) "" reasons
    EmptyDisplayName -> "Display name is missing."
    EmptyFullName -> "Full name is missing."

--------------------------------------------------------------------------------

handler ::
  ( MonadClock m,
    MonadReader env m,
    Has OTEL.Tracer env,
    Has Hasql.Pool.Pool env,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  SockAddr ->
  Maybe Text ->
  Register ->
  m
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler sockAddr mUserAgent req@Register {..} = do
  Observability.handlerSpan "POST /user/register" $ do
    validateRequest req >>= \case
      Failure errors ->
        logValidationFailure "POST /user/register Request validation failure" req errors
      Success parsedRequest -> do
        execQuerySpanThrow (User.getUserByEmail urEmail) >>= \case
          Just _ ->
            logValidationFailure "Email address is already registered." req [InvalidEmailAddress]
          Nothing ->
            registerUser sockAddr mUserAgent parsedRequest

registerUser ::
  ( MonadClock m,
    MonadReader env m,
    Has OTEL.Tracer env,
    Has Hasql.Pool.Pool env,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  SockAddr ->
  Maybe Text ->
  RegisterParsed ->
  m
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
registerUser sockAddr mUserAgent RegisterParsed {..} = do
  Log.logInfo "Registering New User" urpEmail
  OneRow uid <- execQuerySpanThrow $ User.insertUser $ User.ModelInsert urpEmail urpPassword
  _ <- execQuerySpanThrow $ UserMetadata.insertUserMetadata $ UserMetadata.ModelInsert uid urpDisplayName urpFullName Nothing False
  execQuerySpanThrow (User.getUser uid) >>= \case
    Nothing ->
      throwErr Forbidden
    Just _user -> do
      Auth.login uid sockAddr mUserAgent >>= \case
        Left err ->
          throwErr $ InternalServerError $ Text.pack $ show err
        Right sessionId -> do
          pure $ Servant.addHeader (Auth.mkCookieSession sessionId) $ Servant.addHeader "/" Servant.NoContent

parsePassword ::
  ( Monad m,
    MonadIO m
  ) =>
  Password ->
  m (Validation [ValidationError] (PasswordHash Argon2))
parsePassword password =
  case PW.Validate.validatePassword PW.Validate.defaultPasswordPolicy_ password of
    PW.Validate.ValidPassword ->
      Success <$> hashPassword password
    PW.Validate.InvalidPassword reasons ->
      pure $ Failure [InvalidPassword reasons]

validateRequest :: (MonadIO m) => Register -> m (Validation [ValidationError] RegisterParsed)
validateRequest Register {..} = do
  let emailValidation = fromEither $ first (const [InvalidEmailAddress]) $ EmailAddress.validate urEmail
  passwordValidation <- parsePassword urPassword
  pure $ RegisterParsed <$> emailValidation <*> passwordValidation <*> pure urDisplayName <*> pure urFullName

logValidationFailure ::
  ( Log.MonadLog m
  ) =>
  Text ->
  Register ->
  [ValidationError] ->
  m
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
logValidationFailure message req@Register {..} validationErrors = do
  Log.logInfo message (Aeson.object ["request" .= req, "validationErrors" .= display validationErrors])
  pure $ Servant.noHeader $ Servant.addHeader ("/" <> Http.toUrlPiece (userRegisterGetLink (Just urEmail) (Just urDisplayName) (Just urFullName))) Servant.NoContent
