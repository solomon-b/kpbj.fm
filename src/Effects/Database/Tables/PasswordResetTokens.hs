{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @password_reset_tokens@.
--
-- Password reset tokens are used to verify user identity during password
-- recovery. Tokens expire after 1 hour and can only be used once.
module Effects.Database.Tables.PasswordResetTokens
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
    consumeToken,
    expirePendingForUser,
    countRecentForEmail,
    deleteExpired,
    deleteOlderThanDays,
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
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for password reset token primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Token Type

-- | Unique token for password reset.
--
-- Tokens are UUIDs generated when a user requests a password reset and
-- included in the reset email link.
newtype Token = Token {unToken :: Text}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Status Enum

-- | Password reset token status.
data Status
  = -- | Token is awaiting use
    Pending
  | -- | Token has been used to reset password
    Used
  | -- | Token has expired and can no longer be used
    Expired
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded, Read)
  deriving anyclass (FromJSON, ToJSON)

instance Display Status where
  displayBuilder Pending = "pending"
  displayBuilder Used = "used"
  displayBuilder Expired = "expired"

instance DecodeValue Status where
  decodeValue = Decoders.enum decodeStatus

decodeStatus :: Text -> Maybe Status
decodeStatus = \case
  "pending" -> Just Pending
  "used" -> Just Used
  "expired" -> Just Expired
  _ -> Nothing

instance EncodeValue Status where
  encodeValue = Encoders.enum $ \case
    Pending -> "pending"
    Used -> "used"
    Expired -> "expired"

--------------------------------------------------------------------------------
-- Model Type

-- | Password reset token record from the database.
data Model = Model
  { id :: Id,
    userId :: User.Id,
    token :: Token,
    email :: Text,
    status :: Status,
    createdAt :: UTCTime,
    expiresAt :: UTCTime,
    usedAt :: Maybe UTCTime,
    ipAddress :: Maybe Text,
    userAgent :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display Model where
  displayBuilder m = displayBuilder (token m)

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert data for creating a new password reset token.
data Insert = Insert
  { iUserId :: User.Id,
    iToken :: Token,
    iEmail :: Text,
    iIpAddress :: Maybe Text,
    iUserAgent :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Insert a new password reset token and return the ID.
insert :: Insert -> Hasql.Statement () (Maybe Id)
insert Insert {..} =
  interp
    False
    [sql|
    INSERT INTO password_reset_tokens (user_id, token, email, ip_address, user_agent)
    VALUES (#{iUserId}, #{iToken}, #{iEmail}, #{iIpAddress}, #{iUserAgent})
    RETURNING id
  |]

-- | Get a password reset token by its token string.
--
-- Returns a pending, non-expired token if it exists.
-- Does NOT consume the token - use this for validation before showing the reset form.
getByToken :: Token -> Hasql.Statement () (Maybe Model)
getByToken tokenValue =
  interp
    False
    [sql|
    SELECT id, user_id, token, email, status, created_at, expires_at, used_at, ip_address, user_agent
    FROM password_reset_tokens
    WHERE token = #{tokenValue}
      AND status = 'pending'
      AND expires_at > NOW()
  |]

-- | Consume a token, marking it as used.
--
-- This atomically:
-- 1. Marks the token as 'used'
-- 2. Sets the used_at timestamp
--
-- Returns the model if successful, Nothing if:
-- - Token doesn't exist
-- - Token is not in 'pending' status
-- - Token has expired
consumeToken :: Token -> Hasql.Statement () (Maybe Model)
consumeToken tokenValue =
  interp
    False
    [sql|
    UPDATE password_reset_tokens
    SET status = 'used', used_at = NOW()
    WHERE token = #{tokenValue}
      AND status = 'pending'
      AND expires_at > NOW()
    RETURNING id, user_id, token, email, status, created_at, expires_at, used_at, ip_address, user_agent
  |]

-- | Expire all pending tokens for a user.
--
-- Used after a successful password reset to invalidate any other pending tokens.
expirePendingForUser :: User.Id -> Hasql.Statement () ()
expirePendingForUser userId =
  interp
    False
    [sql|
    UPDATE password_reset_tokens
    SET status = 'expired'
    WHERE user_id = #{userId}
      AND status = 'pending'
  |]

-- | Count recent password reset tokens for an email address.
--
-- Used for rate limiting: returns count of tokens created in the last hour.
countRecentForEmail :: Text -> Hasql.Statement () Int64
countRecentForEmail email =
  let query =
        interp
          False
          [sql|
    SELECT COUNT(*)::INT8
    FROM password_reset_tokens
    WHERE email = #{email}
      AND created_at > NOW() - INTERVAL '1 hour'
  |]
   in maybe 0 getOneColumn <$> query

-- | Delete all expired tokens.
--
-- This is used for cleanup jobs. Only deletes pending tokens that have expired.
deleteExpired :: Hasql.Statement () ()
deleteExpired =
  interp
    False
    [sql|
    DELETE FROM password_reset_tokens
    WHERE expires_at < NOW()
      AND status = 'pending'
  |]

-- | Delete all tokens older than the specified number of days.
--
-- This purges old tokens regardless of status (used, expired, pending)
-- for data hygiene. Used to prevent unbounded table growth while maintaining
-- a reasonable audit trail.
deleteOlderThanDays :: Int64 -> Hasql.Statement () ()
deleteOlderThanDays days =
  interp
    False
    [sql|
    DELETE FROM password_reset_tokens
    WHERE created_at < NOW() - (#{days} || ' days')::INTERVAL
  |]
