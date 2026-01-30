{-# LANGUAGE ViewPatterns #-}

-- | Handler for POST /user/forgot-password
--
-- Processes password reset requests. Sends a reset email if the user exists.
-- Always shows the same success message regardless of whether the email exists
-- to prevent email enumeration attacks.
module API.User.ForgotPassword.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.User.ForgotPassword.Get.Templates.Page qualified as Templates
import API.User.ForgotPassword.Post.Route (ForgotPasswordForm (..))
import App.Monad (AppM)
import App.Smtp (SmtpConfig)
import Component.Frame (loadContentOnly, loadFrame)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.EmailAddress (mkEmailAddress)
import Domain.Types.EmailAddress qualified as EmailAddress
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
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

handler ::
  SockAddr ->
  Maybe Text ->
  Maybe HxRequest ->
  ForgotPasswordForm ->
  AppM (Lucid.Html ())
handler sockAddr mUserAgent (foldHxReq -> hxRequest) ForgotPasswordForm {..} = do
  mGoogleAnalyticsId <- asks Has.getter
  mSmtpConfig <- asks Has.getter

  -- Parse the email address to validate it
  case EmailAddress.validate (mkEmailAddress fpfEmail) of
    Left _ -> do
      -- Invalid email format, but still show success to not reveal info
      Log.logInfo "Password reset requested for invalid email format" fpfEmail
      renderSuccess mGoogleAnalyticsId hxRequest
    Right validEmail -> do
      -- Look up the user
      userResult <- execQuery (User.getUserByEmail validEmail)
      case userResult of
        Left err -> do
          Log.logInfo "Password reset lookup failed" (Text.pack $ show err)
          -- Show success anyway to not reveal database issues
          renderSuccess mGoogleAnalyticsId hxRequest
        Right Nothing -> do
          -- User doesn't exist, but show success to prevent enumeration
          Log.logInfo "Password reset requested for non-existent email" (display validEmail)
          renderSuccess mGoogleAnalyticsId hxRequest
        Right (Just user) -> do
          -- User exists, create token and send email
          let ipAddress = Just $ Text.pack $ show sockAddr
          tokenResult <-
            PasswordReset.createPasswordResetToken
              (User.mId user)
              validEmail
              ipAddress
              mUserAgent

          case tokenResult of
            Left err -> do
              Log.logInfo "Password reset token creation failed" (Text.pack $ show err)
              -- Show success anyway to not reveal the error
              renderSuccess mGoogleAnalyticsId hxRequest
            Right token -> do
              -- Send the email
              case mSmtpConfig of
                Nothing -> do
                  -- No SMTP configured, log the token for development
                  Log.logInfo "Password reset token (SMTP not configured)" (ResetTokens.unToken token)
                Just smtpConfig ->
                  sendResetEmail smtpConfig validEmail token

              renderSuccess mGoogleAnalyticsId hxRequest

--------------------------------------------------------------------------------

-- | Render the success page.
--
-- Always shows the same message regardless of outcome.
renderSuccess ::
  Maybe GoogleAnalyticsId ->
  HxRequest ->
  AppM (Lucid.Html ())
renderSuccess mGoogleAnalyticsId hxRequest = do
  let content = Templates.successTemplate
  case hxRequest of
    IsHxRequest -> loadContentOnly content
    IsNotHxRequest -> loadFrame mGoogleAnalyticsId content

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
