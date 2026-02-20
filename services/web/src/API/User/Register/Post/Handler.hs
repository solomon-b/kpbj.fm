module API.User.Register.Post.Handler where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types (UserRoutes (..))
import API.User.Register.Post.Route (Register (..), RegisterParsed (..))
import App.Handler.Error (HandlerError, logHandlerError, throwHandlerFailure)
import App.Monad (AppM)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.List (foldl')
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
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.EmailVerification qualified as EmailVerification
import Hasql.Interpolate (OneRow (..))
import Log qualified
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Network.Socket (SockAddr)
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

-- | Result type for a registration attempt.
data RegisterResult
  = -- | Registration succeeded — redirect URL.
    RegisterSuccess Text
  | -- | Validation or uniqueness check failed — redirect URL.
    RegisterFailure Text

--------------------------------------------------------------------------------

handler ::
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
handler sockAddr mUserAgent req = do
  result <- runExceptT $ action sockAddr mUserAgent req
  case result of
    Left err -> do
      logHandlerError "Register" err
      pure $ Servant.noHeader $ Servant.addHeader ("/" <> Http.toUrlPiece (userLinks.registerGet Nothing Nothing Nothing)) Servant.NoContent
    Right (RegisterSuccess redirectUrl) ->
      pure $ Servant.noHeader $ Servant.addHeader redirectUrl Servant.NoContent
    Right (RegisterFailure redirectUrl) ->
      pure $ Servant.noHeader $ Servant.addHeader redirectUrl Servant.NoContent

--------------------------------------------------------------------------------

-- | Core registration business logic.
--
-- Validates the form, checks for existing users, creates the account,
-- and sends a verification email. Returns a 'RegisterResult'.
action ::
  SockAddr ->
  Maybe Text ->
  Register ->
  ExceptT HandlerError AppM RegisterResult
action sockAddr mUserAgent req@Register {..} = do
  let registerRedirectUrl = "/" <> Http.toUrlPiece (userLinks.registerGet (Just urEmail) (Just urDisplayName) (Just urFullName))
  validationResult <- lift $ validateRequest req
  case validationResult of
    Failure errors -> do
      Log.logInfo "POST /user/register Request validation failure" (Aeson.object ["request" .= req, "validationErrors" .= display errors])
      pure $ RegisterFailure registerRedirectUrl
    Success parsedRequest -> do
      mExisting <- execQueryThrow (User.getUserByEmail urEmail)
      case mExisting of
        Just _ -> do
          Log.logInfo "Email address is already registered." (Aeson.object ["request" .= req, "validationErrors" .= display [InvalidEmailAddress]])
          pure $ RegisterFailure registerRedirectUrl
        Nothing ->
          registerNewUser sockAddr mUserAgent parsedRequest urNewsletter

-- | Create the user account and send the verification email.
registerNewUser ::
  SockAddr ->
  Maybe Text ->
  RegisterParsed ->
  Maybe Bool ->
  ExceptT HandlerError AppM RegisterResult
registerNewUser _sockAddr _mUserAgent RegisterParsed {..} newsletterSubscription = do
  Log.logInfo "Registering New User" urpEmail
  OneRow uid <- execQueryThrow $ User.insertUser $ User.ModelInsert urpEmail urpPassword
  mMetadataId <- execQueryThrow $ UserMetadata.insertUserMetadata $ UserMetadata.Insert uid urpDisplayName urpFullName Nothing UserMetadata.Host UserMetadata.Automatic UserMetadata.DefaultTheme
  case mMetadataId of
    Nothing -> throwHandlerFailure "User metadata insert failed"
    Just _ -> pure ()
  mUser <- execQueryThrow (User.getUser uid)
  case mUser of
    Nothing ->
      throwHandlerFailure "User not found after registration"
    Just _user -> do
      lift $ when (isJust newsletterSubscription) (subscribeToNewsletter urpEmail)
      _ <- lift $ EmailVerification.createAndSendVerification uid urpEmail
      let redirectUrl = "/" <> Http.toUrlPiece (userLinks.verifyEmailSentGet (Just urpEmail))
      pure $ RegisterSuccess redirectUrl

--------------------------------------------------------------------------------

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
