{-# LANGUAGE ViewPatterns #-}

-- | Handler for POST /user/reset-password
--
-- Processes the password reset form submission.
-- Validates the token, checks password requirements, and updates the password.
-- Invalidates all user sessions after successful password change.
module API.User.ResetPassword.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types
import API.User.ResetPassword.Get.Templates.InvalidToken qualified as InvalidTokenTemplate
import API.User.ResetPassword.Get.Templates.Page qualified as Templates
import API.User.ResetPassword.Post.Route (ResetPasswordForm (..))
import App.Monad (AppM)
import Component.Frame (loadContentOnly, loadFrame)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Foldable (foldl')
import Data.Has (getter)
import Data.Password.Argon2 (Argon2, Password, PasswordCheck (..), PasswordHash, checkPassword, hashPassword)
import Data.Password.Validate qualified as PW.Validate
import Data.Text (Text)
import Data.Text.Display (Display (..), display)
import Data.Validation (Validation (..))
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Tables.PasswordResetTokens (Token)
import Effects.PasswordReset qualified as PasswordReset
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace qualified as Trace
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
      "Password is invalid: "
        <> foldl' (\acc x -> acc <> displayBuilder x <> ", ") "" reasons

--------------------------------------------------------------------------------

handler ::
  Trace.Tracer ->
  Maybe HxRequest ->
  ResetPasswordForm ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "HX-Redirect" Text
         ]
        (Lucid.Html ())
    )
handler _tracer (foldHxReq -> hxRequest) ResetPasswordForm {..} = do
  mGoogleAnalyticsId <- asks getter

  -- Validate the password first (pure validation, no DB call).
  -- This avoids unnecessary DB operations if the password is invalid.
  passwordValidation <- validatePassword rpfPassword rpfPasswordConfirm
  case passwordValidation of
    Failure (firstError : _) -> do
      -- Show form with error
      Log.logInfo "Password reset form validation failed" (display firstError)
      content <- renderFormWithError mGoogleAnalyticsId hxRequest rpfToken firstError
      pure $ Servant.noHeader content
    Failure [] -> do
      -- Should never happen, but handle gracefully
      Log.logInfo "Password reset form validation failed with no errors" (display rpfToken)
      content <- renderFormWithError mGoogleAnalyticsId hxRequest rpfToken PasswordMismatch
      pure $ Servant.noHeader content
    Success passwordHash -> do
      -- Atomically consume token and reset password.
      -- This validates the token and consumes it in a single operation,
      -- avoiding TOCTOU race conditions.
      resetResult <- PasswordReset.consumeAndResetPassword rpfToken passwordHash
      case resetResult of
        Left err -> do
          Log.logInfo "Password reset failed" (PasswordReset.passwordResetErrorToText err)
          content <- renderInvalidToken mGoogleAnalyticsId hxRequest
          pure $ Servant.noHeader content
        Right _userId -> do
          Log.logInfo "Password reset successful" (display rpfToken)
          -- Redirect to login with success message
          let redirectUrl = "/" <> Http.toUrlPiece (userLinks.loginGet Nothing Nothing)
          content <- case hxRequest of
            IsHxRequest -> loadContentOnly successContent
            IsNotHxRequest -> loadFrame mGoogleAnalyticsId successContent
          pure $ Servant.addHeader redirectUrl content

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

--------------------------------------------------------------------------------

-- | Render the form with an error message.
renderFormWithError ::
  Maybe GoogleAnalyticsId ->
  HxRequest ->
  Token ->
  ValidationError ->
  AppM (Lucid.Html ())
renderFormWithError mGoogleAnalyticsId hxRequest token err = do
  let errorText = display err
      content = Templates.template token (Just errorText)
  case hxRequest of
    IsHxRequest -> loadContentOnly content
    IsNotHxRequest -> loadFrame mGoogleAnalyticsId content

-- | Render the invalid token page.
renderInvalidToken ::
  Maybe GoogleAnalyticsId ->
  HxRequest ->
  AppM (Lucid.Html ())
renderInvalidToken mGoogleAnalyticsId hxRequest = do
  let content = InvalidTokenTemplate.template
  case hxRequest of
    IsHxRequest -> loadContentOnly content
    IsNotHxRequest -> loadFrame mGoogleAnalyticsId content

-- | Success content (will be replaced by redirect).
successContent :: Lucid.Html ()
successContent =
  Lucid.div_ [Lucid.class_ "min-h-screen flex items-center justify-center"] $ do
    Lucid.div_ [Lucid.class_ "text-center"] $ do
      Lucid.p_ "Password reset successful. Redirecting to login..."
