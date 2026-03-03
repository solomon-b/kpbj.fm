{-# LANGUAGE QuasiQuotes #-}
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
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Lazy qualified as LT
import Domain.Types.EmailAddress (mkEmailAddress)
import Domain.Types.EmailAddress qualified as EmailAddress
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.PasswordResetTokens qualified as ResetTokens
import Effects.Database.Tables.User qualified as User
import App.BaseUrl (baseUrl)
import Effects.Email.Send qualified as Email
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
              lift $ sendResetEmail validEmail token

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
  EmailAddress.EmailAddress ->
  ResetTokens.Token ->
  AppM ()
sendResetEmail email token = do
  let emailText = display email
      tokenText = ResetTokens.unToken token
  url <- baseUrl
  Email.sendAsync (buildPasswordResetEmail url emailText tokenText)
  Log.logInfo "Password reset email queued" emailText

--------------------------------------------------------------------------------

-- | Build a password reset email.
buildPasswordResetEmail ::
  -- | Application base URL
  Text ->
  -- | Recipient email address
  Text ->
  -- | Reset token
  Text ->
  Email.Email
buildPasswordResetEmail appBaseUrl toEmail token =
  let resetUrl = appBaseUrl <> "/user/reset-password?token=" <> token
   in Email.Email
        { Email.emailTo = toEmail,
          Email.emailSubject = "Reset your password - KPBJ 95.9FM",
          Email.emailBody =
            LT.fromStrict
              [i|
================================================================
                  KPBJ 95.9FM COMMUNITY RADIO
================================================================

Password Reset Request

We received a request to reset your password for your
KPBJ 95.9FM account.

RESET YOUR PASSWORD
-------------------
Click or copy this link into your browser:

#{resetUrl}

This link will expire in 1 hour.

DIDN'T REQUEST THIS?
--------------------
If you didn't request a password reset, you can safely
ignore this email. Your password will not be changed
unless you click the link above.

*** SECURITY NOTICE ***
Do not share this link with anyone.

--
The KPBJ Team
https://kpbj.fm

================================================================
|],
          Email.emailLabel = "password-reset"
        }
