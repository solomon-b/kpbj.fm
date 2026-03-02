{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Email sending effect for the application.
--
-- Provides a unified interface for sending emails via SMTP. When SMTP is not
-- configured (development mode), emails are logged to console instead.
--
-- To send an email, build an 'Email' value and pass it to 'sendAsync'.
module Effects.Email.Send
  ( -- * Email Type
    Email (..),

    -- * Sending
    sendAsync,

    -- * URL Building
    baseUrl,
  )
where

--------------------------------------------------------------------------------

import App.Config (Environment (..), Hostname (..), WarpConfig (..))
import App.Monad (AppM)
import App.Smtp (SmtpConfig (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Log qualified
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP qualified as SMTP
import System.IO (hPutStrLn, stderr)

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
-- Reads SMTP configuration from the 'AppM' environment:
--
--   * Configured: sends via SMTP with 30s timeout, logs success/failure
--   * Not configured (dev): logs the email to console
sendAsync :: Email -> AppM ()
sendAsync email = do
  asks Has.getter >>= \case
    Nothing ->
      Log.logInfo [i|Email skipped (SMTP not configured): #{emailLabel email}|] (emailTo email)
    Just config -> do
      let mail = buildMail config email
      Log.logInfo [i|Queuing #{emailLabel email} email|] (emailTo email)
      liftIO $ void $ Async.async $ do
        result <- Async.race (threadDelay (30 * 1000000)) (try $ sendMailWithConfig config mail)
        case result of
          Left () ->
            hPutStrLn stderr [i|[Email] ERROR: Timeout sending #{emailLabel email} to #{emailTo email}|]
          Right (Left (e :: SomeException)) ->
            hPutStrLn stderr [i|[Email] ERROR: Failed #{emailLabel email} to #{emailTo email}: #{show e}|]
          Right (Right ()) ->
            putStrLn [i|[Email] Sent #{emailLabel email} to #{emailTo email}|]

--------------------------------------------------------------------------------

-- | Derive the application base URL from context.
--
-- Uses 'Hostname', 'Environment', and 'WarpConfig' from 'AppM':
--
--   * Development: @http:\/\/hostname:port@
--   * Staging/Production: @https:\/\/hostname@
baseUrl :: AppM Text
baseUrl = do
  Hostname hostname <- asks Has.getter
  env <- asks (Has.getter @Environment)
  WarpConfig {warpConfigPort = port} <- asks Has.getter
  pure $ case env of
    Development -> [i|http://#{hostname}:#{port}|]
    Staging -> [i|https://#{hostname}|]
    Production -> [i|https://#{hostname}|]

--------------------------------------------------------------------------------
-- Internal helpers

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
