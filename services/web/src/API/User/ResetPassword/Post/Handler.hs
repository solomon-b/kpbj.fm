{-# LANGUAGE ViewPatterns #-}

-- | Handler for POST /user/reset-password
--
-- Processes the password reset form submission.
-- Validates the token, checks password requirements, and updates the password.
-- Invalidates all user sessions after successful password change.
module API.User.ResetPassword.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types (UserRoutes (..))
import API.User.ResetPassword.Get.Templates.InvalidToken qualified as InvalidTokenTemplate
import API.User.ResetPassword.Get.Templates.Page qualified as Templates
import API.User.ResetPassword.Post.Route (ResetPasswordForm (..))
import App.Common (renderUnauthTemplate)
import App.Handler.Error (HandlerError, logHandlerError)
import App.Monad (AppM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.List (foldl')
import Data.Password.Argon2 (Argon2, Password, PasswordCheck (..), PasswordHash, checkPassword, hashPassword)
import Data.Password.Validate qualified as PW.Validate
import Data.Text (Text)
import Data.Text.Display (Display (..), display)
import Data.Validation (Validation (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Tables.PasswordResetTokens (Token)
import Effects.PasswordReset qualified as PasswordReset
import Log qualified
import Lucid qualified
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

-- | Result of the password reset action.
data ResetResult
  = -- | Validation or reset failed — render error content.
    ResetRenderContent (Lucid.Html ())
  | -- | Reset succeeded — redirect URL and confirmation content.
    ResetRedirect Text (Lucid.Html ())

--------------------------------------------------------------------------------

-- | Core business logic for password reset form submission.
--
-- Validates the passwords, consumes the token, and resets the password.
-- Returns a 'ResetResult' describing the response to render.
action ::
  HxRequest ->
  ResetPasswordForm ->
  ExceptT HandlerError AppM ResetResult
action hxRequest ResetPasswordForm {..} = do
  passwordValidation <- lift $ validatePassword rpfPassword rpfPasswordConfirm
  case passwordValidation of
    Failure (firstError : _) -> do
      Log.logInfo "Password reset form validation failed" (display firstError)
      content <- lift $ renderFormWithError hxRequest rpfToken firstError
      pure $ ResetRenderContent content
    Failure [] -> do
      Log.logInfo "Password reset form validation failed with no errors" (display rpfToken)
      content <- lift $ renderFormWithError hxRequest rpfToken PasswordMismatch
      pure $ ResetRenderContent content
    Success passwordHash -> do
      resetResult <- lift $ PasswordReset.consumeAndResetPassword rpfToken passwordHash
      case resetResult of
        Left err -> do
          Log.logInfo "Password reset failed" (PasswordReset.passwordResetErrorToText err)
          content <- lift $ renderInvalidToken hxRequest
          pure $ ResetRenderContent content
        Right _userId -> do
          Log.logInfo "Password reset successful" (display rpfToken)
          let redirectUrl = "/" <> Http.toUrlPiece (userLinks.loginGet Nothing Nothing)
          content <- lift $ renderUnauthTemplate hxRequest successContent
          pure $ ResetRedirect redirectUrl content

handler ::
  Maybe HxRequest ->
  ResetPasswordForm ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "HX-Redirect" Text
         ]
        (Lucid.Html ())
    )
handler (foldHxReq -> hxRequest) form = do
  result <- runExceptT $ action hxRequest form
  case result of
    Left err -> do
      logHandlerError "Reset password" err
      content <- renderInvalidToken hxRequest
      pure $ Servant.noHeader content
    Right (ResetRenderContent content) ->
      pure $ Servant.noHeader content
    Right (ResetRedirect redirectUrl content) ->
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
  HxRequest ->
  Token ->
  ValidationError ->
  AppM (Lucid.Html ())
renderFormWithError hxRequest token err = do
  let errorText = display err
      content = Templates.template token (Just errorText)
  renderUnauthTemplate hxRequest content

-- | Render the invalid token page.
renderInvalidToken ::
  HxRequest ->
  AppM (Lucid.Html ())
renderInvalidToken hxRequest = do
  let content = InvalidTokenTemplate.template
  renderUnauthTemplate hxRequest content

-- | Success content (will be replaced by redirect).
successContent :: Lucid.Html ()
successContent =
  Lucid.div_ [Lucid.class_ "min-h-screen flex items-center justify-center"] $ do
    Lucid.div_ [Lucid.class_ "text-center"] $ do
      Lucid.p_ "Password reset successful. Redirecting to login..."
