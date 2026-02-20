{-# LANGUAGE ViewPatterns #-}

-- | Handler for POST /user/forgot-password
--
-- Processes password reset requests. Sends a reset email if the user exists.
-- Always shows the same success message regardless of whether the email exists
-- to prevent email enumeration attacks.
module API.User.ForgotPassword.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.User.ForgotPassword.Get.Templates.Page qualified as Templates
import API.User.ForgotPassword.Post.Route (ForgotPasswordForm (..))
import App.Common (renderUnauthTemplate)
import App.Handler.Error (HandlerError)
import App.Monad (AppM)
import App.Smtp (SmtpConfig)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.EmailAddress (mkEmailAddress)
import Domain.Types.EmailAddress qualified as EmailAddress
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.PasswordResetTokens qualified as ResetTokens
import Effects.Database.Tables.User qualified as User
import Effects.MailSender qualified as MailSender
import Effects.PasswordReset qualified as PasswordReset
import Log qualified
import Lucid qualified
import Network.Socket (SockAddr)

--------------------------------------------------------------------------------

-- | Core business logic for password reset requests.
--
-- Validates the email, looks up the user, creates a reset token, and queues
-- the email. Always succeeds — returns unit regardless of outcome (to prevent
-- email enumeration). Logs any failures internally.
action ::
  SockAddr ->
  Maybe Text ->
  ForgotPasswordForm ->
  ExceptT HandlerError AppM ()
action sockAddr mUserAgent ForgotPasswordForm {..} = do
  mSmtpConfig <- asks Has.getter
  case EmailAddress.validate (mkEmailAddress fpfEmail) of
    Left _ -> do
      Log.logInfo "Password reset requested for invalid email format" fpfEmail
    Right validEmail -> do
      userResult <- execQuery (User.getUserByEmail validEmail)
      case userResult of
        Left err -> do
          Log.logInfo "Password reset lookup failed" (Text.pack $ show err)
        Right Nothing -> do
          Log.logInfo "Password reset requested for non-existent email" (display validEmail)
        Right (Just user) -> do
          let ipAddress = Just $ Text.pack $ show sockAddr
          tokenResult <-
            lift $
              PasswordReset.createPasswordResetToken
                (User.mId user)
                validEmail
                ipAddress
                mUserAgent
          case tokenResult of
            Left err -> do
              Log.logInfo "Password reset token creation failed" (Text.pack $ show err)
            Right token -> do
              lift $ case mSmtpConfig of
                Nothing ->
                  Log.logInfo "Password reset token (SMTP not configured)" (ResetTokens.unToken token)
                Just smtpConfig ->
                  sendResetEmail smtpConfig validEmail token

handler ::
  SockAddr ->
  Maybe Text ->
  Maybe HxRequest ->
  ForgotPasswordForm ->
  AppM (Lucid.Html ())
handler sockAddr mUserAgent (foldHxReq -> hxRequest) form = do
  -- Always show success regardless of outcome (prevents email enumeration)
  _ <- runExceptT $ action sockAddr mUserAgent form
  renderSuccess hxRequest

--------------------------------------------------------------------------------

-- | Render the success page.
--
-- Always shows the same message regardless of outcome.
renderSuccess ::
  HxRequest ->
  AppM (Lucid.Html ())
renderSuccess hxRequest = do
  let content = Templates.successTemplate
  renderUnauthTemplate hxRequest content

--------------------------------------------------------------------------------

-- | Send the password reset email asynchronously.
sendResetEmail ::
  SmtpConfig ->
  EmailAddress.EmailAddress ->
  ResetTokens.Token ->
  AppM ()
sendResetEmail smtpConfig email token = do
  let emailText = display email
      tokenText = ResetTokens.unToken token
  MailSender.sendPasswordResetEmailAsync smtpConfig emailText tokenText
  Log.logInfo "Password reset email queued" emailText
