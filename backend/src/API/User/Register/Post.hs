module API.User.Register.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (userRegisterGetLink)
import App.Auth qualified as Auth
import App.Errors (Forbidden (..), InternalServerError (..), throwErr)
import Control.Monad (when)
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
import Data.Maybe (isJust)
import Data.Password.Argon2 (Argon2, Password, PasswordHash, hashPassword, mkPassword)
import Data.Password.Validate qualified as PW.Validate
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, display)
import Data.Text.Display.Core (Display (..))
import Data.Text.Display.Generic (RecordInstance (..))
import Data.Text.Encoding qualified as Text
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
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
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
    urFullName :: FullName,
    urNewsletter :: Maybe Bool
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
      <*> pure (either (pure Nothing) (const $ Just True) (FormUrlEncoded.parseMaybe @Text "newsletter" f))

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
            registerUser sockAddr mUserAgent parsedRequest urNewsletter

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
  Maybe Bool ->
  m
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
registerUser sockAddr mUserAgent RegisterParsed {..} newsletterSubscription = do
  Log.logInfo "Registering New User" urpEmail
  OneRow uid <- execQuerySpanThrow $ User.insertUser $ User.ModelInsert urpEmail urpPassword
  _ <- execQuerySpanThrow $ UserMetadata.insertUserMetadata $ UserMetadata.ModelInsert uid urpDisplayName urpFullName Nothing False
  execQuerySpanThrow (User.getUser uid) >>= \case
    Nothing ->
      throwErr Forbidden
    Just _user -> do
      when (isJust newsletterSubscription) (subscribeToNewsletter urpEmail)

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

subscribeToNewsletter :: (MonadIO m, Log.MonadLog m, MonadThrow m) => EmailAddress -> m ()
subscribeToNewsletter email = do
  Log.logInfo "Subscribing user to newsletter" email
  let formData = [("entry.936311333", Text.encodeUtf8 $ display email)]
  request' <- HTTP.parseRequest "POST https://docs.google.com/forms/u/0/d/e/1FAIpQLSfeM91iQ_A7ybaa070b8jiznHNRIJ_2JU0F7wJjo7vAvkS3tQ/formResponse"
  let request = HTTP.setRequestBodyURLEncoded formData request'
  response <- HTTP.httpNoBody request
  let status = HTTP.getResponseStatus response
      statusCode = HTTP.statusCode status
  if statusCode >= 200 && statusCode < 300
    then Log.logInfo "Newsletter subscription successful" email
    else Log.logInfo "Newsletter subscription failed" (email, HTTP.statusCode status)

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
