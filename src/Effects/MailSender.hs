{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mail sending effect for the application.
--
-- Provides functionality to send emails via SMTP. When SMTP is not configured
-- (development mode), emails are logged to console instead of being sent.
module Effects.MailSender
  ( -- * Mail Sending
    sendEmail,
    sendVerificationEmail,
    sendVerificationEmailAsync,
    sendPasswordResetEmail,
    sendPasswordResetEmailAsync,
    sendHostAssignmentEmail,
    sendHostAssignmentEmailAsync,

    -- * Mail Building
    buildSimpleMail,
    buildPlainTextMail,

    -- * Host Assignment Email Data
    HostAssignmentInfo (..),
  )
where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import App.Smtp (SmtpConfig (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Has (getter)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Log qualified
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP qualified as SMTP
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------

-- | Send an email using the configured SMTP settings.
--
-- When SMTP is not configured, the email is logged to console instead.
-- Returns True if the email was sent (or logged) successfully, False otherwise.
sendEmail :: Mime.Mail -> AppM Bool
sendEmail mail = do
  mSmtpConfig <- asks getter
  case mSmtpConfig of
    Nothing -> do
      -- SMTP not configured - log the email instead
      Log.logInfo "Email (not sent - SMTP not configured)" $
        Text.pack $
          show (Mime.mailTo mail)
      liftIO $ do
        putStrLn "=== Email (SMTP not configured) ==="
        putStrLn $ "To: " <> show (Mime.mailTo mail)
        putStrLn $ "Subject: " <> getSubjectStr (Mime.mailHeaders mail)
        putStrLn "==================================="
      pure True
    Just config -> do
      Log.logInfo "Sending email" $ Text.pack $ show (Mime.mailTo mail)
      result <- liftIO $ try $ sendMailWithConfig config mail
      case result of
        Left (e :: SomeException) -> do
          Log.logInfo "Failed to send email" $ Text.pack $ show e
          pure False
        Right () -> do
          Log.logInfo "Email sent successfully" $ Text.pack $ show (Mime.mailTo mail)
          pure True
  where
    getSubjectStr :: Mime.Headers -> String
    getSubjectStr [] = "(no subject)"
    getSubjectStr ((name, value) : rest)
      | name == "Subject" = Text.unpack value
      | otherwise = getSubjectStr rest

-- | Send email using SMTP with the given configuration.
--
-- Uses STARTTLS for port 587, implicit TLS for port 465.
sendMailWithConfig :: SmtpConfig -> Mime.Mail -> IO ()
sendMailWithConfig SmtpConfig {..}
  | smtpPort == 465 =
      SMTP.sendMailWithLoginTLS'
        (Text.unpack smtpServer)
        (fromIntegral smtpPort)
        (Text.unpack smtpUsername)
        (Text.unpack smtpPassword)
  | otherwise =
      SMTP.sendMailWithLoginSTARTTLS'
        (Text.unpack smtpServer)
        (fromIntegral smtpPort)
        (Text.unpack smtpUsername)
        (Text.unpack smtpPassword)

--------------------------------------------------------------------------------

-- | Build a simple text/html email.
buildSimpleMail ::
  -- | From email address
  Text ->
  -- | From display name
  Text ->
  -- | To email address
  Text ->
  -- | Subject
  Text ->
  -- | Plain text body
  LT.Text ->
  -- | HTML body
  LT.Text ->
  Mime.Mail
buildSimpleMail fromEmail fromName toEmail subject plainBody htmlBody =
  Mime.Mail
    { Mime.mailFrom = Mime.Address (Just fromName) fromEmail,
      Mime.mailTo = [Mime.Address Nothing toEmail],
      Mime.mailCc = [],
      Mime.mailBcc = [],
      Mime.mailHeaders = [("Subject", subject)],
      Mime.mailParts =
        [ [ Mime.plainPart plainBody,
            Mime.htmlPart htmlBody
          ]
        ]
    }

-- | Build a plain-text-only email (no HTML).
--
-- For that 90s monospace vibe.
buildPlainTextMail ::
  -- | From email address
  Text ->
  -- | From display name
  Text ->
  -- | To email address
  Text ->
  -- | Subject
  Text ->
  -- | Plain text body
  LT.Text ->
  Mime.Mail
buildPlainTextMail fromEmail fromName toEmail subject plainBody =
  Mime.Mail
    { Mime.mailFrom = Mime.Address (Just fromName) fromEmail,
      Mime.mailTo = [Mime.Address Nothing toEmail],
      Mime.mailCc = [],
      Mime.mailBcc = [],
      Mime.mailHeaders = [("Subject", subject)],
      Mime.mailParts = [[Mime.plainPart plainBody]]
    }

--------------------------------------------------------------------------------

-- | Send a verification email to a user.
--
-- Builds and sends an email with a verification link.
sendVerificationEmail ::
  -- | SMTP configuration
  SmtpConfig ->
  -- | Recipient email address
  Text ->
  -- | Verification token
  Text ->
  AppM Bool
sendVerificationEmail config toEmail token = do
  let mail = buildVerificationMail config toEmail token
  sendEmail mail

-- | Send a verification email asynchronously (fire-and-forget).
--
-- Spawns a background thread to send the email so the request handler
-- doesn't block waiting for SMTP. Logs success/failure in the background.
-- Times out after 30 seconds to avoid hanging threads.
sendVerificationEmailAsync ::
  -- | SMTP configuration
  SmtpConfig ->
  -- | Recipient email address
  Text ->
  -- | Verification token
  Text ->
  AppM ()
sendVerificationEmailAsync config toEmail token = do
  let mail = buildVerificationMail config toEmail token
  Log.logInfo "Queuing verification email" toEmail
  liftIO $ void $ Async.async $ do
    putStrLn $ "[Email] Starting send to " <> Text.unpack toEmail
    result <- Async.race (threadDelay (30 * 1000000)) (try $ sendMailWithConfig config mail)
    case result of
      Left () ->
        hPutStrLn stderr $ "[Email] ERROR: Timeout sending to " <> Text.unpack toEmail
      Right (Left (e :: SomeException)) ->
        hPutStrLn stderr $ "[Email] ERROR: Failed to send to " <> Text.unpack toEmail <> ": " <> show e
      Right (Right ()) ->
        putStrLn $ "[Email] Sent verification email to " <> Text.unpack toEmail

-- | Build the verification email.
buildVerificationMail :: SmtpConfig -> Text -> Text -> Mime.Mail
buildVerificationMail config toEmail token =
  let verificationUrl = smtpBaseUrl config <> "/user/verify-email?token=" <> token
      subject = "Verify your email address - KPBJ 95.9FM"
      plainBody =
        LT.fromStrict $
          Text.unlines
            [ "Welcome to KPBJ 95.9FM!",
              "",
              "Please verify your email address by clicking the link below:",
              "",
              verificationUrl,
              "",
              "This link will expire in 24 hours.",
              "",
              "If you didn't create an account with us, you can safely ignore this email.",
              "",
              "Thanks,",
              "The KPBJ Team"
            ]
      htmlBody =
        LT.fromStrict $
          Text.unlines
            [ "<!DOCTYPE html>",
              "<html>",
              "<head>",
              "  <meta charset=\"utf-8\">",
              "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">",
              "</head>",
              "<body style=\"font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; line-height: 1.6; color: #333; max-width: 600px; margin: 0 auto; padding: 20px;\">",
              "  <div style=\"background-color: #1a1a1a; padding: 20px; text-align: center;\">",
              "    <h1 style=\"color: #fff; margin: 0;\">KPBJ 95.9FM</h1>",
              "  </div>",
              "  <div style=\"padding: 30px 20px;\">",
              "    <h2 style=\"color: #1a1a1a; margin-top: 0;\">Welcome to KPBJ!</h2>",
              "    <p>Please verify your email address by clicking the button below:</p>",
              "    <div style=\"text-align: center; margin: 30px 0;\">",
              "      <a href=\"" <> verificationUrl <> "\" style=\"background-color: #1a1a1a; color: #fff; padding: 14px 28px; text-decoration: none; border-radius: 4px; display: inline-block; font-weight: bold;\">Verify Email Address</a>",
              "    </div>",
              "    <p style=\"color: #666; font-size: 14px;\">Or copy and paste this link into your browser:</p>",
              "    <p style=\"color: #666; font-size: 14px; word-break: break-all;\">" <> verificationUrl <> "</p>",
              "    <p style=\"color: #666; font-size: 14px;\">This link will expire in 24 hours.</p>",
              "    <p style=\"color: #666; font-size: 14px;\">If you didn't create an account with us, you can safely ignore this email.</p>",
              "  </div>",
              "  <div style=\"border-top: 1px solid #eee; padding-top: 20px; text-align: center; color: #666; font-size: 12px;\">",
              "    <p>KPBJ 95.9FM Community Radio</p>",
              "  </div>",
              "</body>",
              "</html>"
            ]
   in buildSimpleMail
        (smtpFromEmail config)
        (smtpFromName config)
        toEmail
        subject
        plainBody
        htmlBody

--------------------------------------------------------------------------------

-- | Send a password reset email to a user.
--
-- Builds and sends an email with a password reset link.
sendPasswordResetEmail ::
  -- | SMTP configuration
  SmtpConfig ->
  -- | Recipient email address
  Text ->
  -- | Reset token
  Text ->
  AppM Bool
sendPasswordResetEmail config toEmail token = do
  let mail = buildPasswordResetMail config toEmail token
  sendEmail mail

-- | Send a password reset email asynchronously (fire-and-forget).
--
-- Spawns a background thread to send the email so the request handler
-- doesn't block waiting for SMTP. Logs success/failure in the background.
-- Times out after 30 seconds to avoid hanging threads.
sendPasswordResetEmailAsync ::
  -- | SMTP configuration
  SmtpConfig ->
  -- | Recipient email address
  Text ->
  -- | Reset token
  Text ->
  AppM ()
sendPasswordResetEmailAsync config toEmail token = do
  let mail = buildPasswordResetMail config toEmail token
  Log.logInfo "Queuing password reset email" toEmail
  liftIO $ void $ Async.async $ do
    putStrLn $ "[Email] Starting password reset send to " <> Text.unpack toEmail
    result <- Async.race (threadDelay (30 * 1000000)) (try $ sendMailWithConfig config mail)
    case result of
      Left () ->
        hPutStrLn stderr $ "[Email] ERROR: Timeout sending password reset to " <> Text.unpack toEmail
      Right (Left (e :: SomeException)) ->
        hPutStrLn stderr $ "[Email] ERROR: Failed to send password reset to " <> Text.unpack toEmail <> ": " <> show e
      Right (Right ()) ->
        putStrLn $ "[Email] Sent password reset email to " <> Text.unpack toEmail

-- | Build the password reset email.
buildPasswordResetMail :: SmtpConfig -> Text -> Text -> Mime.Mail
buildPasswordResetMail config toEmail token =
  let resetUrl = smtpBaseUrl config <> "/user/reset-password?token=" <> token
      subject = "Reset your password - KPBJ 95.9FM"
      plainBody =
        LT.fromStrict $
          Text.unlines
            [ "Password Reset Request",
              "",
              "We received a request to reset your password for your KPBJ 95.9FM account.",
              "",
              "Click the link below to reset your password:",
              "",
              resetUrl,
              "",
              "This link will expire in 1 hour.",
              "",
              "If you didn't request a password reset, you can safely ignore this email.",
              "Your password will not be changed unless you click the link above.",
              "",
              "For security reasons, do not share this link with anyone.",
              "",
              "Thanks,",
              "The KPBJ Team"
            ]
      htmlBody =
        LT.fromStrict $
          Text.unlines
            [ "<!DOCTYPE html>",
              "<html>",
              "<head>",
              "  <meta charset=\"utf-8\">",
              "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">",
              "</head>",
              "<body style=\"font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; line-height: 1.6; color: #333; max-width: 600px; margin: 0 auto; padding: 20px;\">",
              "  <div style=\"background-color: #1a1a1a; padding: 20px; text-align: center;\">",
              "    <h1 style=\"color: #fff; margin: 0;\">KPBJ 95.9FM</h1>",
              "  </div>",
              "  <div style=\"padding: 30px 20px;\">",
              "    <h2 style=\"color: #1a1a1a; margin-top: 0;\">Password Reset Request</h2>",
              "    <p>We received a request to reset your password for your KPBJ 95.9FM account.</p>",
              "    <p>Click the button below to reset your password:</p>",
              "    <div style=\"text-align: center; margin: 30px 0;\">",
              "      <a href=\"" <> resetUrl <> "\" style=\"background-color: #1a1a1a; color: #fff; padding: 14px 28px; text-decoration: none; border-radius: 4px; display: inline-block; font-weight: bold;\">Reset Password</a>",
              "    </div>",
              "    <p style=\"color: #666; font-size: 14px;\">Or copy and paste this link into your browser:</p>",
              "    <p style=\"color: #666; font-size: 14px; word-break: break-all;\">" <> resetUrl <> "</p>",
              "    <p style=\"color: #666; font-size: 14px;\">This link will expire in 1 hour.</p>",
              "    <p style=\"color: #666; font-size: 14px;\">If you didn't request a password reset, you can safely ignore this email. Your password will not be changed unless you click the link above.</p>",
              "    <div style=\"background-color: #fff3cd; border: 1px solid #ffc107; padding: 12px; margin-top: 20px; border-radius: 4px;\">",
              "      <p style=\"color: #856404; font-size: 14px; margin: 0;\"><strong>Security Notice:</strong> Do not share this link with anyone.</p>",
              "    </div>",
              "  </div>",
              "  <div style=\"border-top: 1px solid #eee; padding-top: 20px; text-align: center; color: #666; font-size: 12px;\">",
              "    <p>KPBJ 95.9FM Community Radio</p>",
              "  </div>",
              "</body>",
              "</html>"
            ]
   in buildSimpleMail
        (smtpFromEmail config)
        (smtpFromName config)
        toEmail
        subject
        plainBody
        htmlBody

--------------------------------------------------------------------------------
-- Host Assignment Email

-- | Information needed to send a host assignment notification email.
data HostAssignmentInfo = HostAssignmentInfo
  { -- | The host's display name
    haiHostName :: Text,
    -- | The name of the show
    haiShowTitle :: Text,
    -- | Full URL to the show page (built with safe links)
    haiShowUrl :: Text,
    -- | Full URL to the host dashboard (built with safe links)
    haiDashboardUrl :: Text,
    -- | Human-readable timeslot description (e.g., "Fridays 8:00 PM - 10:00 PM PT")
    haiTimeslot :: Maybe Text
  }
  deriving stock (Show, Eq)

-- | Send a host assignment notification email.
--
-- Builds and sends an email welcoming a host to their new show.
sendHostAssignmentEmail ::
  -- | SMTP configuration
  SmtpConfig ->
  -- | Recipient email address
  Text ->
  -- | Host assignment information
  HostAssignmentInfo ->
  AppM Bool
sendHostAssignmentEmail config toEmail info = do
  let mail = buildHostAssignmentMail config toEmail info
  sendEmail mail

-- | Send a host assignment notification email asynchronously (fire-and-forget).
--
-- Spawns a background thread to send the email so the request handler
-- doesn't block waiting for SMTP. Logs success/failure in the background.
-- Times out after 30 seconds to avoid hanging threads.
sendHostAssignmentEmailAsync ::
  -- | SMTP configuration
  SmtpConfig ->
  -- | Recipient email address
  Text ->
  -- | Host assignment information
  HostAssignmentInfo ->
  AppM ()
sendHostAssignmentEmailAsync config toEmail info = do
  let mail = buildHostAssignmentMail config toEmail info
  Log.logInfo "Queuing host assignment email" toEmail
  liftIO $ void $ Async.async $ do
    putStrLn $ "[Email] Starting host assignment send to " <> Text.unpack toEmail
    result <- Async.race (threadDelay (30 * 1000000)) (try $ sendMailWithConfig config mail)
    case result of
      Left () ->
        hPutStrLn stderr $ "[Email] ERROR: Timeout sending host assignment to " <> Text.unpack toEmail
      Right (Left (e :: SomeException)) ->
        hPutStrLn stderr $ "[Email] ERROR: Failed to send host assignment to " <> Text.unpack toEmail <> ": " <> show e
      Right (Right ()) ->
        putStrLn $ "[Email] Sent host assignment email to " <> Text.unpack toEmail

-- | Build the host assignment notification email (plain text only).
buildHostAssignmentMail :: SmtpConfig -> Text -> HostAssignmentInfo -> Mime.Mail
buildHostAssignmentMail config toEmail HostAssignmentInfo {..} =
  let timeslotText = fromMaybe "TBD" haiTimeslot
      subject = "Welcome to " <> haiShowTitle <> " - KPBJ 95.9FM"

      plainBody =
        LT.fromStrict
          [i|
================================================================
                    KPBJ 95.9FM COMMUNITY RADIO
================================================================

Welcome to KPBJ Radio, #{haiHostName}!

You've been added as a host for "#{haiShowTitle}".

YOUR TIMESLOT
-------------
#{timeslotText}

WHAT'S NEXT
-----------
1. Visit your host dashboard:
   #{haiDashboardUrl}

2. Check out your show page:
   #{haiShowUrl}

3. Start preparing your episodes!
   Upload audio, add track listings, write blog posts.

NEED HELP?
----------
Questions about hosting? Reach out to station staff.

We're excited to have you on the airwaves!

--
The KPBJ Team
https://kpbj.fm

================================================================
|]
   in buildPlainTextMail
        (smtpFromEmail config)
        (smtpFromName config)
        toEmail
        subject
        plainBody
