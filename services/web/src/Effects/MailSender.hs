{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mail sending effect for the application.
--
-- Provides functionality to send emails via SMTP. When SMTP is not configured
-- (development mode), emails are logged to console instead of being sent.
module Effects.MailSender
  ( -- * Mail Sending
    sendVerificationEmailAsync,
    sendPasswordResetEmailAsync,
    sendHostAssignmentEmailAsync,

    -- * Mail Building
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

-- | Build the verification email (plain text only).
buildVerificationMail :: SmtpConfig -> Text -> Text -> Mime.Mail
buildVerificationMail config toEmail token =
  let verificationUrl = smtpBaseUrl config <> "/user/verify-email?token=" <> token
      subject = "Verify your email address - KPBJ 95.9FM"
      plainBody =
        LT.fromStrict
          [i|
================================================================
                    KPBJ 95.9FM COMMUNITY RADIO
================================================================

Welcome to KPBJ Radio!

Please verify your email address to complete your registration.

VERIFY YOUR EMAIL
-----------------
Click or copy this link into your browser:

#{verificationUrl}

This link will expire in 24 hours.

DIDN'T SIGN UP?
---------------
If you didn't create an account with us, you can safely
ignore this email.

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

--------------------------------------------------------------------------------

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

-- | Build the password reset email (plain text only).
buildPasswordResetMail :: SmtpConfig -> Text -> Text -> Mime.Mail
buildPasswordResetMail config toEmail token =
  let resetUrl = smtpBaseUrl config <> "/user/reset-password?token=" <> token
      subject = "Reset your password - KPBJ 95.9FM"
      plainBody =
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
|]
   in buildPlainTextMail
        (smtpFromEmail config)
        (smtpFromName config)
        toEmail
        subject
        plainBody

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
