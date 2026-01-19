{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @email_verification_tokens@.
--
-- Email verification tokens are used to verify user email addresses during
-- registration. Tokens expire after 24 hours and can only be used once.
module Effects.Database.Tables.EmailVerificationTokens
  ( -- * Id Type
    Id (..),

    -- * Token Type
    Token (..),

    -- * Status Enum
    Status (..),

    -- * Model Type
    Model (..),

    -- * Insert Type
    Insert (..),

    -- * Queries
    insert,
    getByToken,
    verifyToken,
    invalidateForUser,
    getLatestPendingForUser,
    deleteExpired,

    -- * User Verification Status
    isUserEmailVerified,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for email verification token primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Token Type

-- | Unique token for email verification.
--
-- Tokens are UUIDs generated when a user registers and included in the
-- verification email link.
newtype Token = Token {unToken :: Text}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Status Enum

-- | Verification token status.
data Status
  = -- | Token is awaiting verification
    Pending
  | -- | Token has been used to verify email
    Verified
  | -- | Token has expired and can no longer be used
    Expired
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded, Read)
  deriving anyclass (FromJSON, ToJSON)

instance Display Status where
  displayBuilder Pending = "pending"
  displayBuilder Verified = "verified"
  displayBuilder Expired = "expired"

instance DecodeValue Status where
  decodeValue = Decoders.enum decodeStatus

decodeStatus :: Text -> Maybe Status
decodeStatus = \case
  "pending" -> Just Pending
  "verified" -> Just Verified
  "expired" -> Just Expired
  _ -> Nothing

instance EncodeValue Status where
  encodeValue = Encoders.enum $ \case
    Pending -> "pending"
    Verified -> "verified"
    Expired -> "expired"

--------------------------------------------------------------------------------
-- Model Type

-- | Email verification token record from the database.
data Model = Model
  { id :: Id,
    userId :: User.Id,
    token :: Token,
    email :: Text,
    status :: Status,
    createdAt :: UTCTime,
    expiresAt :: UTCTime,
    verifiedAt :: Maybe UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display Model where
  displayBuilder m = displayBuilder (token m)

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert data for creating a new verification token.
data Insert = Insert
  { iUserId :: User.Id,
    iToken :: Token,
    iEmail :: Text
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Insert a new verification token and return the ID.
insert :: Insert -> Hasql.Statement () (Maybe Id)
insert Insert {..} =
  interp
    False
    [sql|
    INSERT INTO email_verification_tokens (user_id, token, email)
    VALUES (#{iUserId}, #{iToken}, #{iEmail})
    RETURNING id
  |]

-- | Get a verification token by its token string.
--
-- Returns Nothing if the token doesn't exist.
getByToken :: Token -> Hasql.Statement () (Maybe Model)
getByToken token =
  interp
    False
    [sql|
    SELECT id, user_id, token, email, status, created_at, expires_at, verified_at
    FROM email_verification_tokens
    WHERE token = #{token}
  |]

-- | Verify a token, marking it as verified and updating the user.
--
-- This atomically:
-- 1. Marks the token as 'verified'
-- 2. Sets the verified_at timestamp
-- 3. Updates the user's email_verified and email_verified_at fields
--
-- Returns the model if successful, Nothing if:
-- - Token doesn't exist
-- - Token is not in 'pending' status
-- - Token has expired
verifyToken :: Token -> Hasql.Statement () (Maybe Model)
verifyToken token =
  interp
    False
    [sql|
    WITH updated_token AS (
      UPDATE email_verification_tokens
      SET status = 'verified', verified_at = NOW()
      WHERE token = #{token}
        AND status = 'pending'
        AND expires_at > NOW()
      RETURNING id, user_id, token, email, status, created_at, expires_at, verified_at
    ),
    update_user AS (
      UPDATE users
      SET email_verified = TRUE, email_verified_at = NOW()
      WHERE id = (SELECT user_id FROM updated_token)
    )
    SELECT id, user_id, token, email, status, created_at, expires_at, verified_at
    FROM updated_token
  |]

-- | Invalidate (expire) all pending tokens for a user.
--
-- Used when creating a new token to ensure only one valid token exists.
-- Returns Nothing (we don't care about the count of affected rows).
invalidateForUser :: User.Id -> Hasql.Statement () ()
invalidateForUser userId =
  interp
    False
    [sql|
    UPDATE email_verification_tokens
    SET status = 'expired'
    WHERE user_id = #{userId}
      AND status = 'pending'
  |]

-- | Get the latest pending verification token for a user.
--
-- Returns Nothing if no pending token exists.
getLatestPendingForUser :: User.Id -> Hasql.Statement () (Maybe Model)
getLatestPendingForUser userId =
  interp
    False
    [sql|
    SELECT id, user_id, token, email, status, created_at, expires_at, verified_at
    FROM email_verification_tokens
    WHERE user_id = #{userId}
      AND status = 'pending'
      AND expires_at > NOW()
    ORDER BY created_at DESC
    LIMIT 1
  |]

-- | Delete all expired tokens.
--
-- This is used for cleanup jobs.
deleteExpired :: Hasql.Statement () ()
deleteExpired =
  interp
    False
    [sql|
    DELETE FROM email_verification_tokens
    WHERE expires_at < NOW()
      AND status = 'pending'
  |]

--------------------------------------------------------------------------------
-- User Verification Status

-- | Check if a user's email is verified.
--
-- Queries the email_verified column on the users table.
-- Returns Just User.Id if verified, Nothing otherwise.
isUserEmailVerified :: User.Id -> Hasql.Statement () (Maybe User.Id)
isUserEmailVerified userId =
  interp
    False
    [sql|
    SELECT id
    FROM users
    WHERE id = #{userId}
      AND email_verified = TRUE
  |]
