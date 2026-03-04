-- | Email sending effect.
--
-- Provides a unified interface for sending emails via SMTP. When SMTP is not
-- configured (development mode), emails are logged to console instead.
--
-- This module is polymorphic in the monad @m@, requiring only 'MonadIO',
-- 'MonadReader' with a 'Has (Maybe SmtpConfig)' environment, and 'MonadLog'.
--
-- To send an email, build an 'Email' value and pass it to 'sendAsync'.
module Effects.Email.Send
  ( -- * Email Type
    Email (..),

    -- * Sending
    send,
    sendAsync,
    sendMail,

    -- * MIME Construction
    buildMail,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Has qualified as Has
import Data.Aeson qualified as Aeson
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.Time (getCurrentTime)
import Effects.Email.Config (SmtpConfig (..))
import Log qualified
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP qualified as SMTP

--------------------------------------------------------------------------------

-- | A fully-described, ready-to-send email.
data Email = Email
  { -- | Recipient email address
    emailTo :: Text,
    -- | Subject line
    emailSubject :: Text,
    -- | Plain text body
    emailBody :: LT.Text,
    -- | Label for logging (e.g., "verification", "password-reset")
    emailLabel :: Text
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- | Send an email asynchronously (fire-and-forget).
--
-- Reads SMTP configuration from the environment via 'Has':
--
--   * Configured: sends via SMTP with 30s timeout, logs success/failure
--   * Not configured (dev): logs the email to console
sendAsync ::
  (MonadIO m, MonadReader r m, Has.Has (Maybe SmtpConfig) r, Log.MonadLog m) =>
  Email ->
  m ()
sendAsync email = do
  asks Has.getter >>= \case
    Nothing ->
      Log.logInfo [i|Email skipped (SMTP not configured): #{emailLabel email}|] (emailTo email)
    Just config -> do
      let mail = buildMail config email
      logEnv <- Log.getLoggerEnv
      Log.logInfo [i|Queuing #{emailLabel email} email|] (emailTo email)
      liftIO $ void $ Async.async $ do
        result <- Async.race (threadDelay (30 * 1000000)) (try $ sendMail config mail)
        case result of
          Left () ->
            logIO logEnv Log.LogAttention [i|Timeout sending #{emailLabel email} email|] (emailTo email)
          Right (Left (e :: SomeException)) ->
            logIO logEnv Log.LogAttention [i|Failed sending #{emailLabel email} email: #{show e}|] (emailTo email)
          Right (Right ()) ->
            logIO logEnv Log.LogInfo [i|Sent #{emailLabel email} email|] (emailTo email)

-- | Send an email synchronously.
--
-- Builds and sends the email in the current thread. Suitable for batch jobs
-- where you want to ensure delivery before the process exits.
send :: SmtpConfig -> Email -> IO ()
send config = sendMail config . buildMail config

--------------------------------------------------------------------------------

-- | Build a 'Mime.Mail' value from SMTP config and an 'Email'.
buildMail :: SmtpConfig -> Email -> Mime.Mail
buildMail SmtpConfig {..} Email {..} =
  Mime.Mail
    { Mime.mailFrom = Mime.Address (Just smtpFromName) smtpFromEmail,
      Mime.mailTo = [Mime.Address Nothing emailTo],
      Mime.mailCc = [],
      Mime.mailBcc = [],
      Mime.mailHeaders = [("Subject", emailSubject)],
      Mime.mailParts = [[Mime.plainPart emailBody]]
    }

-- | Send email using SMTP with the given configuration.
--
-- Uses STARTTLS for port 587, implicit TLS for port 465.
sendMail :: SmtpConfig -> Mime.Mail -> IO ()
sendMail SmtpConfig {..}
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

-- | Log a message from plain IO using a captured 'LoggerEnv'.
--
-- Used in async threads that have escaped the 'MonadLog' context.
logIO :: (Aeson.ToJSON a) => Log.LoggerEnv -> Log.LogLevel -> Text -> a -> IO ()
logIO logEnv level msg payload = do
  now <- getCurrentTime
  Log.logMessageIO logEnv now level msg (Aeson.toJSON payload)
