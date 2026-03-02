{-# LANGUAGE QuasiQuotes #-}

-- | Email verification service.
--
-- Provides operations for:
-- - Creating verification tokens for new users
-- - Verifying email addresses via token
-- - Resending verification emails
-- - Cleaning up expired tokens
module Effects.EmailVerification
  ( -- * Token Generation
    generateVerificationToken,

    -- * Verification Operations
    createAndSendVerification,
    verifyEmail,
    resendVerification,

    -- * Error Types
    VerificationError (..),
    verificationErrorToText,
  )
where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Lazy qualified as LT
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EmailVerificationTokens qualified as VerificationTokens
import Effects.Database.Tables.User qualified as User
import Effects.Email.Send qualified as Email
import Log qualified

--------------------------------------------------------------------------------
-- Token Generation

-- | Generate a cryptographically secure token for email verification.
--
-- Uses UUID v4 (random) which is generated from a cryptographically secure
-- random number generator. The token is formatted as a 32-character hex string
-- (UUID without hyphens) for URL-safety and compact storage.
generateVerificationToken :: (MonadIO m) => m VerificationTokens.Token
generateVerificationToken = liftIO $ do
  uuid <- UUID.V4.nextRandom
  -- Remove hyphens from UUID for cleaner tokens
  let tokenText = Text.filter (/= '-') $ UUID.toText uuid
  pure $ VerificationTokens.Token tokenText

--------------------------------------------------------------------------------
-- Error Types

-- | Errors that can occur during email verification operations.
data VerificationError
  = -- | Database query failed
    VerificationDbError Text
  | -- | Token not found, expired, or already used
    TokenInvalid
  | -- | User not found
    UserNotFound
  | -- | Failed to send verification email
    EmailSendFailed
  | -- | SMTP not configured
    SmtpNotConfigured
  | -- | Rate limited - includes seconds until next request allowed
    RateLimited Int
  | -- | Email is already verified
    AlreadyVerified
  deriving stock (Show, Eq)

-- | Cooldown period between resend requests in seconds.
resendCooldownSeconds :: Int64
resendCooldownSeconds = 60

-- | Convert a VerificationError to user-friendly text.
verificationErrorToText :: VerificationError -> Text
verificationErrorToText = \case
  VerificationDbError _ -> "An error occurred. Please try again."
  TokenInvalid -> "This verification link is invalid or has expired. Please request a new one."
  UserNotFound -> "User account not found."
  EmailSendFailed -> "Failed to send verification email. Please try again."
  SmtpNotConfigured -> "Email service is not configured."
  RateLimited secs -> "Please wait " <> Text.pack (show secs) <> " seconds before requesting another email."
  AlreadyVerified -> "Your email is already verified."

--------------------------------------------------------------------------------
-- Verification Operations

-- | Create a verification token and send the verification email.
--
-- This function:
-- 1. Invalidates any existing pending tokens for the user
-- 2. Creates a new verification token
-- 3. Sends the verification email
--
-- Returns the token on success for tracking purposes.
createAndSendVerification ::
  -- | User ID
  User.Id ->
  -- | User email address
  EmailAddress ->
  AppM (Either VerificationError VerificationTokens.Token)
createAndSendVerification userId emailAddress = do
  -- Generate token
  token <- generateVerificationToken

  -- Invalidate any existing pending tokens for this user
  void $ execQuery (VerificationTokens.invalidateForUser userId)

  -- Create the token in the database
  let tokenInsert =
        VerificationTokens.Insert
          { VerificationTokens.iUserId = userId,
            VerificationTokens.iToken = token,
            VerificationTokens.iEmail = display emailAddress
          }

  execQuery (VerificationTokens.insert tokenInsert) >>= \case
    Left err -> do
      Log.logInfo "Failed to create verification token" (Text.pack $ show err)
      pure $ Left $ VerificationDbError "Failed to create verification token"
    Right Nothing -> do
      Log.logInfo "Failed to create verification token (no ID returned)" (display emailAddress)
      pure $ Left $ VerificationDbError "Failed to create verification token"
    Right (Just tokenId) -> do
      -- Send the verification email asynchronously (fire-and-forget)
      url <- Email.baseUrl
      Email.sendAsync (buildVerificationEmail url (display emailAddress) (VerificationTokens.unToken token))
      Log.logInfo "Verification email queued" (Aeson.object ["email" .= display emailAddress, "tokenId" .= tokenId])
      pure $ Right token

-- | Verify an email address using a token.
--
-- This function:
-- 1. Validates the token (exists, pending, not expired)
-- 2. Marks the token as verified
-- 3. Updates the user's email_verified flag
--
-- Returns the user ID and verified email address on success.
verifyEmail ::
  -- | Verification token
  Text ->
  AppM (Either VerificationError (User.Id, Text))
verifyEmail tokenText = do
  let token = VerificationTokens.Token tokenText

  -- Attempt to verify the token (this also updates the user)
  result <- execQuery (VerificationTokens.verifyToken token)
  case result of
    Left err -> do
      Log.logInfo "Failed to verify email (database error)" (Text.pack $ show err)
      pure $ Left $ VerificationDbError "Failed to verify email"
    Right Nothing -> do
      Log.logInfo "Failed to verify email (token invalid or expired)" tokenText
      pure $ Left TokenInvalid
    Right (Just verifiedToken) -> do
      Log.logInfo "Email verified successfully" (VerificationTokens.evtEmail verifiedToken)
      pure $ Right (VerificationTokens.evtUserId verifiedToken, VerificationTokens.evtEmail verifiedToken)

-- | Resend verification email for a user.
--
-- Creates a new token and sends the verification email.
-- Invalidates any existing pending tokens.
-- Rate limited to one request per 'resendCooldownSeconds'.
-- Returns 'AlreadyVerified' if the user's email is already verified.
resendVerification ::
  -- | User ID
  User.Id ->
  -- | User email address
  EmailAddress ->
  AppM (Either VerificationError ())
resendVerification userId email = do
  -- Check if already verified first
  verifiedResult <- execQuery (VerificationTokens.isUserEmailVerified userId)
  case verifiedResult of
    Left err -> do
      Log.logInfo "Failed to check verification status" (Text.pack $ show err)
      pure $ Left $ VerificationDbError "Failed to check verification status"
    Right (Just _) -> do
      -- User is already verified
      Log.logInfo "Resend skipped - email already verified" (display userId)
      pure $ Left AlreadyVerified
    Right Nothing -> do
      -- User not verified, proceed with rate limit check and resend
      rateLimitResult <- checkResendRateLimit userId
      case rateLimitResult of
        Left err -> pure $ Left err
        Right () -> do
          result <- createAndSendVerification userId email
          case result of
            Left err -> pure $ Left err
            Right _ -> pure $ Right ()

-- | Check if the user is rate limited for resending verification emails.
--
-- Returns Left RateLimited if rate limited, Right () if the request is allowed.
checkResendRateLimit :: User.Id -> AppM (Either VerificationError ())
checkResendRateLimit userId = do
  -- Check if a token was created within the cooldown period
  recentTokenResult <- execQuery (VerificationTokens.getLastTokenCreatedAt userId resendCooldownSeconds)
  case recentTokenResult of
    Left err -> do
      Log.logInfo "Failed to check rate limit" (Text.pack $ show err)
      -- Allow the request if we can't check - fail open for UX
      pure $ Right ()
    Right Nothing ->
      -- No recent token, allow the request
      pure $ Right ()
    Right (Just _) -> do
      -- Recent token exists, rate limit
      Log.logInfo "Resend rate limited" (display userId)
      pure $ Left $ RateLimited (fromIntegral resendCooldownSeconds)

--------------------------------------------------------------------------------

-- | Build a verification email.
buildVerificationEmail ::
  -- | Application base URL
  Text ->
  -- | Recipient email address
  Text ->
  -- | Verification token
  Text ->
  Email.Email
buildVerificationEmail appBaseUrl toEmail token =
  let verificationUrl = appBaseUrl <> "/user/verify-email?token=" <> token
   in Email.Email
        { Email.emailTo = toEmail,
          Email.emailSubject = "Verify your email address - KPBJ 95.9FM",
          Email.emailBody =
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
|],
          Email.emailLabel = "verification"
        }
