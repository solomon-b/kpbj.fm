module API.User.Register.Post.Handler where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types
import API.User.Register.Post.Route (Register (..), RegisterParsed (..))
import App.Errors (Forbidden (..), throwErr)
import App.Monad (AppM)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Maybe (isJust)
import Data.Password.Argon2 (Argon2, Password, PasswordHash, hashPassword)
import Data.Password.Validate qualified as PW.Validate
import Data.Text (Text)
import Data.Text.Display (Display, display)
import Data.Text.Display.Core (Display (..))
import Data.Text.Encoding qualified as Text
import Data.Validation
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.EmailAddress qualified as EmailAddress
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.EmailVerification qualified as EmailVerification
import Hasql.Interpolate (OneRow (..))
import Log qualified
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Network.Socket (SockAddr)
import OpenTelemetry.Trace qualified as OTEL
import OrphanInstances.OneRow ()
import OrphanInstances.Servant ()
import Servant qualified
import Web.HttpApiData qualified as Http

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
  OTEL.Tracer ->
  SockAddr ->
  Maybe Text ->
  Register ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler _tracer sockAddr mUserAgent req@Register {..} = do
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
  SockAddr ->
  Maybe Text ->
  RegisterParsed ->
  Maybe Bool ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
registerUser _sockAddr _mUserAgent RegisterParsed {..} newsletterSubscription = do
  Log.logInfo "Registering New User" urpEmail
  OneRow uid <- execQuerySpanThrow $ User.insertUser $ User.ModelInsert urpEmail urpPassword
  _ <- execQuerySpanThrow $ UserMetadata.insertUserMetadata $ UserMetadata.Insert uid urpDisplayName urpFullName Nothing UserMetadata.Host UserMetadata.Automatic UserMetadata.DefaultTheme
  execQuerySpanThrow (User.getUser uid) >>= \case
    Nothing ->
      throwErr Forbidden
    Just _user -> do
      when (isJust newsletterSubscription) (subscribeToNewsletter urpEmail)

      -- Send verification email instead of auto-login
      _ <- EmailVerification.createAndSendVerification uid urpEmail

      -- Redirect to the "check your email" page
      let redirectUrl = "/" <> Http.toUrlPiece (userLinks.verifyEmailSentGet (Just urpEmail))
      pure $ Servant.noHeader $ Servant.addHeader redirectUrl Servant.NoContent

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
  -- Sanitize user profile fields to prevent XSS attacks
  -- Note: DisplayName and FullName are already validated types, so we just pass them through
  -- The sanitization will occur when these values are displayed in templates

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
  Text ->
  Register ->
  [ValidationError] ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Text,
           Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
logValidationFailure message req@Register {..} validationErrors = do
  Log.logInfo message (Aeson.object ["request" .= req, "validationErrors" .= display validationErrors])
  pure $ Servant.noHeader $ Servant.addHeader ("/" <> Http.toUrlPiece (userLinks.registerGet (Just urEmail) (Just urDisplayName) (Just urFullName))) Servant.NoContent
