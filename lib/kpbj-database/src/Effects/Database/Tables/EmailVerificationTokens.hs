{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @email_verification_tokens@.
--
-- Email verification tokens are used to verify user email addresses during
-- registration. Tokens expire after 24 hours and can only be used once.
--
-- Uses rel8 for type-safe database queries where possible.
module Effects.Database.Tables.EmailVerificationTokens
  ( -- * Id Type
    Id (..),

    -- * Token Type
    Token (..),

    -- * Status Enum
    Status (..),

    -- * Table Definition
    EmailVerificationToken (..),
    emailVerificationTokenSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    insert,
    verifyToken,
    invalidateForUser,
    getLastTokenCreatedAt,
    deleteExpired,
    deleteOlderThanDays,

    -- * User Verification Status
    isUserEmailVerified,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Enum, Insert, insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for email verification token primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq)
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
  deriving newtype (Show, Eq, Ord, DBType, DBEq)
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

instance DBType Status where
  typeInformation =
    parseTypeInformation
      ( \case
          "pending" -> Right Pending
          "verified" -> Right Verified
          "expired" -> Right Expired
          other -> Left $ "Invalid EmailVerificationTokens.Status: " <> Text.unpack other
      )
      ( \case
          Pending -> "pending"
          Verified -> "verified"
          Expired -> "expired"
      )
      typeInformation

instance DBEq Status

--------------------------------------------------------------------------------
-- Table Definition

-- | The @email_verification_tokens@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data EmailVerificationToken f = EmailVerificationToken
  { evtId :: Column f Id,
    evtUserId :: Column f User.Id,
    evtToken :: Column f Token,
    evtEmail :: Column f Text,
    evtStatus :: Column f Status,
    evtCreatedAt :: Column f UTCTime,
    evtExpiresAt :: Column f UTCTime,
    evtVerifiedAt :: Column f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (EmailVerificationToken f)

deriving stock instance (f ~ Result) => Eq (EmailVerificationToken f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (EmailVerificationToken Result)

-- | Display instance for EmailVerificationToken Result.
instance Display (EmailVerificationToken Result) where
  displayBuilder m = displayBuilder (evtToken m)

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @EmailVerificationToken Result@.
type Model = EmailVerificationToken Result

-- | Table schema connecting the Haskell type to the database table.
emailVerificationTokenSchema :: TableSchema (EmailVerificationToken Name)
emailVerificationTokenSchema =
  TableSchema
    { name = "email_verification_tokens",
      columns =
        EmailVerificationToken
          { evtId = "id",
            evtUserId = "user_id",
            evtToken = "token",
            evtEmail = "email",
            evtStatus = "status",
            evtCreatedAt = "created_at",
            evtExpiresAt = "expires_at",
            evtVerifiedAt = "verified_at"
          }
    }

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
  fmap listToMaybe $
    run $
      Rel8.insert
        Rel8.Insert
          { into = emailVerificationTokenSchema,
            rows =
              values
                [ EmailVerificationToken
                    { evtId = nextId "email_verification_tokens_id_seq",
                      evtUserId = lit iUserId,
                      evtToken = lit iToken,
                      evtEmail = lit iEmail,
                      evtStatus = lit Pending,
                      evtCreatedAt = now,
                      evtExpiresAt = unsafeDefault, -- DB default: NOW() + INTERVAL '24 hours'
                      evtVerifiedAt = Rel8.null
                    }
                ],
            onConflict = Abort,
            returning = Returning evtId
          }

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
  run_ $
    update
      Rel8.Update
        { target = emailVerificationTokenSchema,
          from = pure (),
          set = \_ row ->
            row
              { evtStatus = lit Expired
              },
          updateWhere = \_ row ->
            evtUserId row ==. lit userId
              &&. evtStatus row ==. lit Pending,
          returning = NoReturning
        }

-- | Check if a token was created within the given number of seconds.
--
-- Used for rate limiting - returns True if a token exists that was created
-- within the specified cooldown period.
getLastTokenCreatedAt :: User.Id -> Int64 -> Hasql.Statement () (Maybe Id)
getLastTokenCreatedAt userId cooldownSeconds =
  interp
    False
    [sql|
    SELECT id
    FROM email_verification_tokens
    WHERE user_id = #{userId}
      AND created_at > NOW() - (#{cooldownSeconds} || ' seconds')::INTERVAL
    ORDER BY created_at DESC
    LIMIT 1
  |]

-- | Delete all expired tokens.
--
-- This is used for cleanup jobs. Only deletes pending tokens that have expired.
deleteExpired :: Hasql.Statement () ()
deleteExpired =
  run_ $
    delete
      Rel8.Delete
        { from = emailVerificationTokenSchema,
          using = pure (),
          deleteWhere = \_ row ->
            evtExpiresAt row <. now
              &&. evtStatus row ==. lit Pending,
          returning = NoReturning
        }

-- | Delete all tokens older than the specified number of days.
--
-- This purges old tokens regardless of status (verified, expired, pending)
-- for data hygiene. Used to prevent unbounded table growth while maintaining
-- a reasonable audit trail.
deleteOlderThanDays :: Int64 -> Hasql.Statement () ()
deleteOlderThanDays days =
  interp
    False
    [sql|
    DELETE FROM email_verification_tokens
    WHERE created_at < NOW() - (#{days} || ' days')::INTERVAL
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
