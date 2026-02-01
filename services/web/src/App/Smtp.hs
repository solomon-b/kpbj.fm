-- | SMTP configuration for sending emails.
--
-- This module provides SMTP configuration loading from environment variables.
-- When SMTP is not configured (development mode), email operations will be
-- logged instead of sent.
--
-- Gmail App Password Setup:
-- 1. Enable 2FA on Gmail account
-- 2. Go to https://myaccount.google.com/apppasswords
-- 3. Generate App Password for "Mail"
-- 4. Use the 16-character code as APP_SMTP_PASSWORD
module App.Smtp
  ( -- * SMTP Configuration
    SmtpConfig (..),
    initSmtpConfig,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (lookupEnv)

--------------------------------------------------------------------------------

-- | SMTP configuration for sending emails.
--
-- All fields are required for email sending to work.
-- If SMTP is not configured, email operations log to console instead.
data SmtpConfig = SmtpConfig
  { -- | SMTP server hostname (e.g., "smtp.gmail.com")
    smtpServer :: Text,
    -- | SMTP server port (typically 587 for TLS)
    smtpPort :: Int,
    -- | SMTP authentication username
    smtpUsername :: Text,
    -- | SMTP authentication password (use App Password for Gmail)
    smtpPassword :: Text,
    -- | "From" email address for outgoing emails
    smtpFromEmail :: Text,
    -- | "From" display name for outgoing emails
    smtpFromName :: Text,
    -- | Base URL for the application (used in email links)
    smtpBaseUrl :: Text
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- | Initialize SMTP configuration from environment variables.
--
-- Environment variables:
--   - APP_SMTP_SERVER: SMTP server hostname (e.g., "smtp.gmail.com")
--   - APP_SMTP_PORT: SMTP server port (e.g., "587")
--   - APP_SMTP_USERNAME: SMTP authentication username
--   - APP_SMTP_PASSWORD: SMTP authentication password
--   - APP_SMTP_FROM_EMAIL: "From" email address
--   - APP_SMTP_FROM_NAME: "From" display name
--   - APP_BASE_URL: Base URL for email links (e.g., "https://kpbj.fm")
--
-- Returns Nothing if any required environment variables are missing.
-- When SMTP is not configured, emails will be logged to console instead.
initSmtpConfig :: (MonadIO m) => m (Maybe SmtpConfig)
initSmtpConfig = liftIO $ do
  mServer <- lookupEnvText "APP_SMTP_SERVER"
  mPort <- lookupEnvInt "APP_SMTP_PORT"
  mUsername <- lookupEnvText "APP_SMTP_USERNAME"
  mPassword <- lookupEnvText "APP_SMTP_PASSWORD"
  mFromEmail <- lookupEnvText "APP_SMTP_FROM_EMAIL"
  mFromName <- lookupEnvText "APP_SMTP_FROM_NAME"
  mBaseUrl <- lookupEnvText "APP_BASE_URL"

  case (mServer, mPort, mUsername, mPassword, mFromEmail, mFromName, mBaseUrl) of
    (Just server, Just port, Just username, Just password, Just fromEmail, Just fromName, Just baseUrl) -> do
      putStrLn $ "SMTP configured (server: " <> Text.unpack server <> ":" <> show port <> ")"
      pure $
        Just
          SmtpConfig
            { smtpServer = server,
              smtpPort = port,
              smtpUsername = username,
              smtpPassword = password,
              smtpFromEmail = fromEmail,
              smtpFromName = fromName,
              smtpBaseUrl = baseUrl
            }
    _ -> do
      putStrLn "SMTP not configured - emails will be logged to console"
      pure Nothing

-- | Look up an environment variable and convert to Text.
lookupEnvText :: String -> IO (Maybe Text)
lookupEnvText name = do
  mVal <- lookupEnv name
  pure $ Text.pack <$> mVal

-- | Look up an environment variable and convert to Int.
lookupEnvInt :: String -> IO (Maybe Int)
lookupEnvInt name = do
  mVal <- lookupEnv name
  pure $ mVal >>= readMaybe
  where
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing
