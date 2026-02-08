{-# LANGUAGE QuasiQuotes #-}

-- | Password reset service.
--
-- Provides operations for:
-- - Creating password reset tokens
-- - Validating and consuming reset tokens
-- - Updating user passwords
-- - Invalidating all user sessions after password change
module Effects.PasswordReset
  ( -- * Token Generation
    generateResetToken,

    -- * Rate Limiting
    checkRateLimit,
    maxRequestsPerHour,

    -- * Password Reset Operations
    createPasswordResetToken,
    validateToken,
    consumeAndResetPassword,

    -- * Error Types
    PasswordResetError (..),
    passwordResetErrorToText,
  )
where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int64)
import Data.Password.Argon2 (Argon2, PasswordHash (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.PasswordResetTokens qualified as ResetTokens
import Effects.Database.Tables.User qualified as User
import Hasql.Interpolate (OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Log qualified

--------------------------------------------------------------------------------
-- Constants

-- | Maximum password reset requests per email per hour.
maxRequestsPerHour :: Int64
maxRequestsPerHour = 3

--------------------------------------------------------------------------------
-- Token Generation

-- | Generate a cryptographically secure token for password reset.
--
-- Uses UUID v4 (random) which is generated from a cryptographically secure
-- random number generator. The token is formatted as a 32-character hex string
-- (UUID without hyphens) for URL-safety and compact storage.
generateResetToken :: (MonadIO m) => m ResetTokens.Token
generateResetToken = liftIO $ do
  uuid <- UUID.V4.nextRandom
  -- Remove hyphens from UUID for cleaner tokens
  let tokenText = Text.filter (/= '-') $ UUID.toText uuid
  pure $ ResetTokens.Token tokenText

--------------------------------------------------------------------------------
-- Error Types

-- | Errors that can occur during password reset operations.
data PasswordResetError
  = -- | Database query failed
    ResetDbError Text
  | -- | Token not found, expired, or already used
    TokenInvalid
  | -- | User not found
    UserNotFound
  | -- | Failed to send reset email
    EmailSendFailed
  | -- | SMTP not configured
    SmtpNotConfigured
  | -- | Too many reset requests
    RateLimitExceeded
  | -- | Failed to update password
    PasswordUpdateFailed
  deriving stock (Show, Eq)

-- | Convert a PasswordResetError to user-friendly text.
passwordResetErrorToText :: PasswordResetError -> Text
passwordResetErrorToText = \case
  ResetDbError _ -> "An error occurred. Please try again."
  TokenInvalid -> "This password reset link is invalid or has expired. Please request a new one."
  UserNotFound -> "User account not found."
  EmailSendFailed -> "Failed to send password reset email. Please try again."
  SmtpNotConfigured -> "Email service is not configured."
  RateLimitExceeded -> "Too many password reset requests. Please try again later."
  PasswordUpdateFailed -> "Failed to update password. Please try again."

--------------------------------------------------------------------------------
-- Rate Limiting

-- | Check if an email has exceeded the rate limit for password reset requests.
--
-- Returns True if the email can request a new token, False if rate limited.
checkRateLimit :: Text -> AppM (Either PasswordResetError Bool)
checkRateLimit email = do
  result <- execQuery (ResetTokens.countRecentForEmail email)
  case result of
    Left err -> do
      Log.logInfo "Rate limit check failed" (Text.pack $ show err)
      pure $ Left $ ResetDbError "Failed to check rate limit"
    Right count ->
      pure $ Right $ count < maxRequestsPerHour

--------------------------------------------------------------------------------
-- Password Reset Operations

-- | Create a password reset token for a user.
--
-- This function:
-- 1. Checks rate limiting
-- 2. Expires any existing pending tokens for the user
-- 3. Creates a new reset token
--
-- Returns the token on success for sending in the email.
-- Does NOT send the email - caller is responsible for that.
createPasswordResetToken ::
  -- | User ID
  User.Id ->
  -- | User email address
  EmailAddress ->
  -- | IP address (for audit logging)
  Maybe Text ->
  -- | User agent (for audit logging)
  Maybe Text ->
  AppM (Either PasswordResetError ResetTokens.Token)
createPasswordResetToken userId email ipAddress userAgent = do
  let emailText = display email

  -- Check rate limit
  checkRateLimit emailText >>= \case
    Left err -> pure $ Left err
    Right False -> do
      Log.logInfo "Password reset rate limit exceeded" emailText
      pure $ Left RateLimitExceeded
    Right True -> do
      -- Generate token
      token <- generateResetToken

      -- Expire any existing pending tokens for this user
      _ <- execQuery (ResetTokens.expirePendingForUser userId)

      -- Create the token in the database
      let tokenInsert =
            ResetTokens.Insert
              { ResetTokens.iUserId = userId,
                ResetTokens.iToken = token,
                ResetTokens.iEmail = emailText,
                ResetTokens.iIpAddress = ipAddress,
                ResetTokens.iUserAgent = userAgent
              }

      insertResult <- execQuery (ResetTokens.insert tokenInsert)
      case insertResult of
        Left err -> do
          Log.logInfo "Failed to create password reset token" (Text.pack $ show err)
          pure $ Left $ ResetDbError "Failed to create password reset token"
        Right Nothing -> do
          Log.logInfo "Failed to create password reset token (no ID returned)" emailText
          pure $ Left $ ResetDbError "Failed to create password reset token"
        Right (Just _) -> do
          Log.logInfo "Password reset token created" emailText
          pure $ Right token

-- | Validate a password reset token without consuming it.
--
-- Used to check if a token is valid before showing the password reset form.
-- Returns the token model if valid.
validateToken ::
  -- | Token from URL
  ResetTokens.Token ->
  AppM (Either PasswordResetError ResetTokens.Model)
validateToken token = do
  result <- execQuery (ResetTokens.getByToken token)
  case result of
    Left err -> do
      Log.logInfo "Token validation failed (database error)" (Text.pack $ show err)
      pure $ Left $ ResetDbError "Failed to validate token"
    Right Nothing -> do
      Log.logInfo "Token validation failed (invalid or expired)" (display token)
      pure $ Left TokenInvalid
    Right (Just tokenModel) -> do
      Log.logInfo "Token validated successfully" (display token)
      pure $ Right tokenModel

-- | Consume a token and reset the user's password.
--
-- This atomically:
-- 1. Marks the token as used
-- 2. Updates the user's password
-- 3. Expires all other pending reset tokens for the user
-- 4. Invalidates all user sessions (forces re-login)
--
-- Returns the user ID on success.
consumeAndResetPassword ::
  -- | Token from form
  ResetTokens.Token ->
  -- | New password hash
  PasswordHash Argon2 ->
  AppM (Either PasswordResetError User.Id)
consumeAndResetPassword token newPasswordHash = do
  -- Consume the token
  consumeResult <- execQuery (ResetTokens.consumeToken token)
  case consumeResult of
    Left err -> do
      Log.logInfo "Token consumption failed (database error)" (Text.pack $ show err)
      pure $ Left $ ResetDbError "Failed to consume token"
    Right Nothing -> do
      Log.logInfo "Token consumption failed (invalid or already used)" (display token)
      pure $ Left TokenInvalid
    Right (Just tokenModel) -> do
      let userId = ResetTokens.prtUserId tokenModel

      -- Update the user's password
      updateResult <- execQuery (updateUserPassword userId newPasswordHash)
      case updateResult of
        Left err -> do
          Log.logInfo "Password update failed" (Text.pack $ show err)
          pure $ Left PasswordUpdateFailed
        Right Nothing -> do
          Log.logInfo "Password update failed (user not found)" (display userId)
          pure $ Left UserNotFound
        Right (Just _) -> do
          -- Expire all other pending tokens for this user
          _ <- execQuery (ResetTokens.expirePendingForUser userId)

          -- Invalidate all sessions for this user
          _ <- execQuery (deleteAllSessionsForUser userId)

          Log.logInfo "Password reset successful" (display userId)
          pure $ Right userId

--------------------------------------------------------------------------------
-- Direct Database Queries
--
-- These queries are defined here because they are specific to password reset
-- functionality and not available in the web-server-core library.

-- | Update a user's password directly.
--
-- Used for password reset where we don't know the old password.
updateUserPassword :: User.Id -> PasswordHash Argon2 -> Hasql.Statement () (Maybe User.Id)
updateUserPassword userId (PasswordHash newHash) =
  interp
    False
    [sql|
    UPDATE users
    SET password = #{newHash}
    WHERE id = #{userId}
      AND deleted_at IS NULL
    RETURNING id
  |]

-- | Delete all sessions for a user.
--
-- Used after password reset to force re-authentication on all devices.
deleteAllSessionsForUser :: User.Id -> Hasql.Statement () Int64
deleteAllSessionsForUser userId =
  let query =
        interp
          False
          [sql|
    WITH deleted AS (
      DELETE FROM server_sessions
      WHERE user_id = #{userId}
      RETURNING 1
    )
    SELECT COUNT(*)::INT8 FROM deleted
  |]
   in maybe 0 getOneColumn <$> query
