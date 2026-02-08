{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @password_reset_tokens@.
--
-- Password reset tokens are used to verify user identity during password
-- recovery. Tokens expire after 1 hour and can only be used once.
--
-- Uses rel8 for type-safe database queries where possible.
module Effects.Database.Tables.PasswordResetTokens
  ( -- * Id Type
    Id (..),

    -- * Token Type
    Token (..),

    -- * Status Enum
    Status (..),

    -- * Table Definition
    PasswordResetToken (..),
    passwordResetTokenSchema,

    -- * Model (Result alias)
    Model,

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
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Enum, Insert, insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for password reset token primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq)
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
  deriving newtype (Show, Eq, Ord, DBType, DBEq)
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
  deriving (DBType) via (Rel8.Enum Status)

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

instance Rel8.DBEnum Status where
  enumTypeName = Rel8.QualifiedName {Rel8.name = "password_reset_status", Rel8.schema = Nothing}
  enumValue Pending = "pending"
  enumValue Used = "used"
  enumValue Expired = "expired"

instance DBEq Status

--------------------------------------------------------------------------------
-- Table Definition

-- | The @password_reset_tokens@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data PasswordResetToken f = PasswordResetToken
  { prtId :: Column f Id,
    prtUserId :: Column f User.Id,
    prtToken :: Column f Token,
    prtEmail :: Column f Text,
    prtStatus :: Column f Status,
    prtCreatedAt :: Column f UTCTime,
    prtExpiresAt :: Column f UTCTime,
    prtUsedAt :: Column f (Maybe UTCTime),
    prtIpAddress :: Column f (Maybe Text),
    prtUserAgent :: Column f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (PasswordResetToken f)

deriving stock instance (f ~ Result) => Eq (PasswordResetToken f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (PasswordResetToken Result)

-- | Display instance for PasswordResetToken Result.
instance Display (PasswordResetToken Result) where
  displayBuilder m = displayBuilder (prtToken m)

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @PasswordResetToken Result@.
type Model = PasswordResetToken Result

-- | Table schema connecting the Haskell type to the database table.
passwordResetTokenSchema :: TableSchema (PasswordResetToken Name)
passwordResetTokenSchema =
  TableSchema
    { name = "password_reset_tokens",
      columns =
        PasswordResetToken
          { prtId = "id",
            prtUserId = "user_id",
            prtToken = "token",
            prtEmail = "email",
            prtStatus = "status",
            prtCreatedAt = "created_at",
            prtExpiresAt = "expires_at",
            prtUsedAt = "used_at",
            prtIpAddress = "ip_address",
            prtUserAgent = "user_agent"
          }
    }

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
  fmap listToMaybe $
    run $
      Rel8.insert
        Rel8.Insert
          { into = passwordResetTokenSchema,
            rows =
              values
                [ PasswordResetToken
                    { prtId = nextId "password_reset_tokens_id_seq",
                      prtUserId = lit iUserId,
                      prtToken = lit iToken,
                      prtEmail = lit iEmail,
                      prtStatus = lit Pending,
                      prtCreatedAt = now,
                      prtExpiresAt = unsafeDefault, -- DB default: NOW() + INTERVAL '1 hour'
                      prtUsedAt = Rel8.null,
                      prtIpAddress = lit iIpAddress,
                      prtUserAgent = lit iUserAgent
                    }
                ],
            onConflict = Abort,
            returning = Returning prtId
          }

-- | Get a password reset token by its token string.
--
-- Returns a pending, non-expired token if it exists.
-- Does NOT consume the token - use this for validation before showing the reset form.
getByToken :: Token -> Hasql.Statement () (Maybe Model)
getByToken tokenValue = fmap listToMaybe $ run $ select do
  row <- each passwordResetTokenSchema
  where_ $ prtToken row ==. lit tokenValue
  where_ $ prtStatus row ==. lit Pending
  where_ $ prtExpiresAt row >. now
  pure row

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
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = passwordResetTokenSchema,
            from = pure (),
            set = \_ row ->
              row
                { prtStatus = lit Used,
                  prtUsedAt = nullify now
                },
            updateWhere = \_ row ->
              prtToken row ==. lit tokenValue
                &&. prtStatus row ==. lit Pending
                &&. prtExpiresAt row >. now,
            returning = Returning Prelude.id
          }

-- | Expire all pending tokens for a user.
--
-- Used after a successful password reset to invalidate any other pending tokens.
expirePendingForUser :: User.Id -> Hasql.Statement () ()
expirePendingForUser userId =
  run_ $
    update
      Rel8.Update
        { target = passwordResetTokenSchema,
          from = pure (),
          set = \_ row ->
            row
              { prtStatus = lit Expired
              },
          updateWhere = \_ row ->
            prtUserId row ==. lit userId
              &&. prtStatus row ==. lit Pending,
          returning = NoReturning
        }

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
  run_ $
    delete
      Rel8.Delete
        { from = passwordResetTokenSchema,
          using = pure (),
          deleteWhere = \_ row ->
            prtExpiresAt row <. now
              &&. prtStatus row ==. lit Pending,
          returning = NoReturning
        }

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
