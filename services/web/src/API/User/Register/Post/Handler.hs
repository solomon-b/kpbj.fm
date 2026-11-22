module API.User.Register.Post.Handler where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types (UserRoutes (..))
import API.User.Register.Post.Route (Register (..), RegisterParsed (..))
import App.Handler.Error (HandlerError, logHandlerError, throwHandlerFailure)
import App.Monad (AppM)
import Component.Banner (BannerType (Error))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Maybe (isJust)
import Data.Password.Argon2 (Argon2, Password, PasswordHash, hashPassword)
import Data.Password.Validate qualified as PW.Validate
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, display)
import Data.Text.Display.Core (Display (..))
import Data.Validation
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.EmailVerification qualified as EmailVerification
import Effects.Mailchimp.Sync (SyncOp (..), syncAsync)
import Hasql.Interpolate (OneRow (..))
import Log qualified
import Network.Socket (SockAddr)
import OrphanInstances.OneRow ()
import OrphanInstances.Servant ()
import Servant qualified
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

data ValidationError
  = InvalidPassword [PW.Validate.InvalidReason]
  | EmptyDisplayName
  | EmptyFullName

instance Display ValidationError where
  displayBuilder = \case
    InvalidPassword reasons -> "Password is invalid: " <> fold (intersperse " " (map displayBuilder reasons))
    EmptyDisplayName -> "Display name is missing."
    EmptyFullName -> "Full name is missing."

--------------------------------------------------------------------------------

-- | Result type for a registration attempt.
data RegisterResult
  = -- | Registration succeeded — redirect URL.
    RegisterSuccess Text
  | -- | Validation or uniqueness check failed. Has the redirect URL and the
    -- banner that tells the user why.
    RegisterFailure Text FlashMessage

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
      pure $
        Servant.addHeader (flashCookie (Just $ registrationFailed "Something went wrong. Try again.")) $
          Servant.addHeader (registerPageUrl req) Servant.NoContent
    Right (RegisterSuccess redirectUrl) ->
      pure $ Servant.noHeader $ Servant.addHeader redirectUrl Servant.NoContent
    Right (RegisterFailure redirectUrl flash) ->
      pure $ Servant.addHeader (flashCookie (Just flash)) $ Servant.addHeader redirectUrl Servant.NoContent

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
  let registerRedirectUrl = registerPageUrl req
  validationResult <- lift $ validateRequest req
  case validationResult of
    Failure errors -> do
      Log.logInfo "POST /user/register Request validation failure" (Aeson.object ["email" .= urEmail, "displayName" .= urDisplayName, "validationErrors" .= display errors])
      pure $ RegisterFailure registerRedirectUrl $ registrationFailed $ Text.unwords $ map display errors
    Success parsedRequest -> do
      mExisting <- execQueryThrow (User.getUserByEmail urEmail)
      case mExisting of
        Just _ -> do
          Log.logInfo "Email address is already registered." (Aeson.object ["email" .= urEmail])
          pure $ RegisterFailure registerRedirectUrl $ registrationFailed "Your account could not be created. Check your details and try again."
        Nothing ->
          registerNewUser sockAddr mUserAgent parsedRequest urNewsletter

-- | The register page, filled in with the fields of the attempt.
registerPageUrl :: Register -> Text
registerPageUrl Register {..} =
  "/" <> Http.toUrlPiece (userLinks.registerGet (Just urEmail) (Just urDisplayName) (Just urFullName))

-- | The banner that the register page shows after a failed registration.
registrationFailed :: Text -> FlashMessage
registrationFailed = FlashMessage Error "Registration Failed"

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
  mMetadataId <- execQueryThrow $ UserMetadata.insertUserMetadata $ UserMetadata.Insert uid urpDisplayName urpFullName Nothing UserMetadata.User UserMetadata.Automatic UserMetadata.DefaultTheme
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
  passwordValidation <- parsePassword urPassword
  pure $ RegisterParsed urEmail <$> passwordValidation <*> pure urDisplayName <*> pure urFullName

-- | Specialized to 'AppM' rather than the prior polymorphic
-- @(MonadLog m, MonadThrow m, MonadDB m) => m ()@ because 'syncAsync'
-- additionally requires @MonadIO@, @MonadReader r m@, and two @Has@
-- constraints. There is exactly one caller — the registration handler —
-- so the polymorphism was abstraction without a second consumer.
subscribeToNewsletter :: EmailAddress -> AppM ()
subscribeToNewsletter email = do
  Log.logInfo "Subscribing user to newsletter" email
  mSubId <- execQueryThrow (NewsletterSubscribers.insert (NewsletterSubscribers.Insert email))
  forM_ mSubId $ \subId -> syncAsync (Upsert email subId)
  Log.logInfo "Newsletter subscription recorded" email
