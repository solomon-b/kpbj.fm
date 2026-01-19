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

    -- * Mail Building
    buildSimpleMail,
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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Log qualified
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP qualified as SMTP

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
        putStrLn $ "[Email] Timeout sending to " <> Text.unpack toEmail
      Right (Left (e :: SomeException)) ->
        putStrLn $ "[Email] Failed to send to " <> Text.unpack toEmail <> ": " <> show e
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
