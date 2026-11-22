-- | Handler for POST /user/reset-password
--
-- Processes the password reset form submission.
-- Validates the token, checks password requirements, and updates the password.
-- Invalidates all user sessions after successful password change.
module API.User.ResetPassword.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types (UserRoutes (..))
import API.User.ResetPassword.Post.Route (ResetPasswordForm (..))
import App.Handler.Error (HandlerError, logHandlerError)
import App.Monad (AppM)
import Component.Banner qualified as Banner
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Password.Argon2 (Argon2, Password, PasswordCheck (..), PasswordHash, checkPassword, hashPassword)
import Data.Password.Validate qualified as PW.Validate
import Data.Text (Text)
import Data.Text.Display (Display (..), display)
import Data.Validation (Validation (..))
import Effects.Database.Tables.PasswordResetTokens (Token)
import Effects.PasswordReset qualified as PasswordReset
import Log qualified
import Servant qualified
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

-- | Validation errors for password reset form.
data ValidationError
  = PasswordMismatch
  | InvalidPassword [PW.Validate.InvalidReason]
  deriving stock (Show)

instance Display ValidationError where
  displayBuilder = \case
    PasswordMismatch -> "Passwords do not match."
    InvalidPassword reasons ->
      "Password is invalid: " <> fold (intersperse " " (map displayBuilder reasons))

--------------------------------------------------------------------------------

-- | Core business logic for password reset form submission.
--
-- Validates the passwords, consumes the token, and resets the password.
-- Returns the redirect URL and the flash message for the target page.
action ::
  ResetPasswordForm ->
  ExceptT HandlerError AppM (Text, Maybe FlashMessage)
action ResetPasswordForm {..} = do
  passwordValidation <- lift $ validatePassword rpfPassword rpfPasswordConfirm
  case passwordValidation of
    Failure errors -> do
      let err = case errors of
            firstError : _ -> firstError
            [] -> PasswordMismatch
      Log.logInfo "Password reset form validation failed" (display err)
      pure (resetPageUrl rpfToken, Just $ resetFailed $ display err)
    Success passwordHash -> do
      resetResult <- lift $ PasswordReset.consumeAndResetPassword rpfToken passwordHash
      case resetResult of
        Left PasswordReset.TokenInvalid -> do
          -- The reset page shows the invalid token page for this token.
          Log.logInfo "Password reset failed" (PasswordReset.passwordResetErrorToText PasswordReset.TokenInvalid)
          pure (resetPageUrl rpfToken, Nothing)
        Left err -> do
          Log.logInfo "Password reset failed" (PasswordReset.passwordResetErrorToText err)
          pure (resetPageUrl rpfToken, Just resetError)
        Right _userId -> do
          Log.logInfo "Password reset successful" (display rpfToken)
          let loginUrl = "/" <> Http.toUrlPiece (userLinks.loginGet Nothing Nothing)
          pure (loginUrl, Just resetSucceeded)

handler ::
  ResetPasswordForm ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "HX-Redirect" Text,
           Servant.Header "Set-Cookie" Text
         ]
        Servant.NoContent
    )
handler form = do
  result <- runExceptT $ action form
  (redirectUrl, mFlash) <- case result of
    Left err -> do
      logHandlerError "Reset password" err
      pure (resetPageUrl form.rpfToken, Just resetError)
    Right r -> pure r
  pure $
    Servant.addHeader redirectUrl $
      maybe Servant.noHeader (Servant.addHeader . flashCookie . Just) mFlash Servant.NoContent

--------------------------------------------------------------------------------

-- | The reset password page for a token.
resetPageUrl :: Token -> Text
resetPageUrl token = "/" <> Http.toUrlPiece (userLinks.resetPasswordGet (Just token))

-- | The banner that the reset password page shows after a failed reset.
resetFailed :: Text -> FlashMessage
resetFailed = FlashMessage Banner.Error "Password Not Reset"

-- | The banner for a reset that failed for a reason other than the form.
resetError :: FlashMessage
resetError = resetFailed "Something went wrong, and your password did not change. Try again."

-- | The banner that the login page shows after a successful reset.
resetSucceeded :: FlashMessage
resetSucceeded = FlashMessage Banner.Success "Password Reset" "Your password was changed. Log in with your new password."

--------------------------------------------------------------------------------

-- | Validate the password and password confirmation.
--
-- Validates that:
-- 1. The password meets the password policy requirements
-- 2. The password and confirmation match
--
-- Uses the password library's checkPassword to compare passwords securely
-- by hashing the first password and verifying the second against it.
validatePassword ::
  Password ->
  Password ->
  AppM (Validation [ValidationError] (PasswordHash Argon2))
validatePassword password passwordConfirm =
  -- First validate the password meets policy requirements
  case PW.Validate.validatePassword PW.Validate.defaultPasswordPolicy_ password of
    PW.Validate.InvalidPassword reasons ->
      pure $ Failure [InvalidPassword reasons]
    PW.Validate.ValidPassword -> do
      -- Hash the first password (generates salt + hash)
      hash <- liftIO $ hashPassword password
      -- Verify the confirmation matches using the same salt
      case checkPassword passwordConfirm hash of
        PasswordCheckFail -> pure $ Failure [PasswordMismatch]
        PasswordCheckSuccess -> pure $ Success hash
