{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @host_invitations@.
--
-- Host invitations are created by staff to onboard new hosts. Each invitation
-- contains a unique token and pre-configured schedule data. Tokens expire
-- after 7 days and are single-use.
--
-- Uses rel8 for type-safe database queries where possible.
module Effects.Database.Tables.HostInvitation
  ( -- * Id Type
    Id (..),

    -- * Token Type
    Token (..),

    -- * Status Enum
    Status (..),

    -- * Table Definition
    HostInvitation (..),
    hostInvitationSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Result Types
    ModelWithCreator (..),

    -- * Queries
    insert,
    getByToken,
    getAll,
    getAllWithCreator,
    claimInvitation,
    revokeInvitation,
    updateRecipientEmail,
    getById,

    -- * Token Generation
    generateInviteCode,
  )
where

--------------------------------------------------------------------------------

import Control.Monad (replicateM)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Domain.Types.EmailAddress (EmailAddress)
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
import System.Random qualified as Random

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for host invitation primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Token Type

-- | Unique token for host invitation.
--
-- Tokens are UUIDs generated when a staff member creates an invitation and
-- included in the onboarding link sent to the new host.
newtype Token = Token {unToken :: Text}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, DBType, DBEq)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Status Enum

-- | Host invitation status.
data Status
  = -- | Invitation is awaiting use
    Pending
  | -- | Invitation has been claimed by a new host
    Claimed
  | -- | Invitation has expired and can no longer be used
    Expired
  | -- | Invitation was manually revoked by staff
    Revoked
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded, Read)
  deriving anyclass (FromJSON, ToJSON)
  deriving (DBType) via (Rel8.Enum Status)

instance Display Status where
  displayBuilder Pending = "pending"
  displayBuilder Claimed = "claimed"
  displayBuilder Expired = "expired"
  displayBuilder Revoked = "revoked"

instance DecodeValue Status where
  decodeValue = Decoders.enum decodeStatus

decodeStatus :: Text -> Maybe Status
decodeStatus = \case
  "pending" -> Just Pending
  "claimed" -> Just Claimed
  "expired" -> Just Expired
  "revoked" -> Just Revoked
  _ -> Nothing

instance EncodeValue Status where
  encodeValue = Encoders.enum $ \case
    Pending -> "pending"
    Claimed -> "claimed"
    Expired -> "expired"
    Revoked -> "revoked"

instance Rel8.DBEnum Status where
  enumTypeName = Rel8.QualifiedName {Rel8.name = "host_invitation_status", Rel8.schema = Nothing}
  enumValue Pending = "pending"
  enumValue Claimed = "claimed"
  enumValue Expired = "expired"
  enumValue Revoked = "revoked"

instance DBEq Status

--------------------------------------------------------------------------------
-- Table Definition

-- | The @host_invitations@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data HostInvitation f = HostInvitation
  { hiId :: Column f Id,
    hiToken :: Column f Token,
    hiStatus :: Column f Status,
    hiScheduleData :: Column f Value,
    hiRecipientEmail :: Column f EmailAddress,
    hiCreatedBy :: Column f User.Id,
    hiClaimedBy :: Column f (Maybe User.Id),
    hiClaimedAt :: Column f (Maybe UTCTime),
    hiCreatedAt :: Column f UTCTime,
    hiExpiresAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (HostInvitation f)

deriving stock instance (f ~ Result) => Eq (HostInvitation f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (HostInvitation Result)

-- | Display instance for HostInvitation Result.
instance Display (HostInvitation Result) where
  displayBuilder inv =
    "HostInvitation { id = "
      <> displayBuilder (hiId inv)
      <> ", token = "
      <> displayBuilder (hiToken inv)
      <> ", status = "
      <> displayBuilder (hiStatus inv)
      <> " }"

-- | Type alias for convenience.
--
-- @Model@ is the same as @HostInvitation Result@.
type Model = HostInvitation Result

-- | Table schema connecting the Haskell type to the database table.
hostInvitationSchema :: TableSchema (HostInvitation Name)
hostInvitationSchema =
  TableSchema
    { name = "host_invitations",
      columns =
        HostInvitation
          { hiId = "id",
            hiToken = "token",
            hiStatus = "status",
            hiScheduleData = "schedule_data",
            hiRecipientEmail = "recipient_email",
            hiCreatedBy = "created_by",
            hiClaimedBy = "claimed_by",
            hiClaimedAt = "claimed_at",
            hiCreatedAt = "created_at",
            hiExpiresAt = "expires_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert data for creating a new host invitation.
data Insert = Insert
  { iToken :: Token,
    iScheduleData :: Value,
    iRecipientEmail :: EmailAddress,
    iCreatedBy :: User.Id
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Result Types

-- | Host invitation with creator and claimer display names for list views.
--
-- Used by 'getAllWithCreator' which joins with @user_metadata@ to resolve
-- user IDs to display names.
data ModelWithCreator = ModelWithCreator
  { mwcId :: Id,
    mwcToken :: Token,
    mwcStatus :: Status,
    mwcScheduleData :: Value,
    mwcRecipientEmail :: EmailAddress,
    mwcCreatedByName :: Text,
    mwcClaimedByName :: Maybe Text,
    mwcCreatedAt :: UTCTime,
    mwcExpiresAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

-- | Display instance for ModelWithCreator.
instance Display ModelWithCreator where
  displayBuilder mwc =
    "ModelWithCreator { id = "
      <> displayBuilder (mwcId mwc)
      <> ", status = "
      <> displayBuilder (mwcStatus mwc)
      <> ", createdBy = "
      <> displayBuilder (mwcCreatedByName mwc)
      <> " }"

--------------------------------------------------------------------------------
-- Queries

-- | Insert a new host invitation and return the ID.
insert :: Insert -> Hasql.Statement () (Maybe Id)
insert Insert {..} =
  fmap listToMaybe $
    run $
      Rel8.insert
        Rel8.Insert
          { into = hostInvitationSchema,
            rows =
              values
                [ HostInvitation
                    { hiId = nextId "host_invitations_id_seq",
                      hiToken = lit iToken,
                      hiStatus = lit Pending,
                      hiScheduleData = lit iScheduleData,
                      hiRecipientEmail = lit iRecipientEmail,
                      hiCreatedBy = lit iCreatedBy,
                      hiClaimedBy = Rel8.null,
                      hiClaimedAt = Rel8.null,
                      hiCreatedAt = now,
                      hiExpiresAt = unsafeDefault -- DB default: NOW() + INTERVAL '7 days'
                    }
                ],
            onConflict = Abort,
            returning = Returning hiId
          }

-- | Get a host invitation by its token string.
--
-- Returns a pending, non-expired invitation if it exists.
getByToken :: Token -> Hasql.Statement () (Maybe Model)
getByToken tokenValue =
  interp
    False
    [sql|
    SELECT id, token, status, schedule_data, recipient_email, created_by, claimed_by, claimed_at, created_at, expires_at
    FROM host_invitations
    WHERE token = #{tokenValue}
      AND status = 'pending'
      AND expires_at > NOW()
  |]

-- | Get all host invitations ordered by creation date (newest first).
getAll :: Hasql.Statement () [Model]
getAll =
  interp
    True
    [sql|
    SELECT id, token, status, schedule_data, recipient_email, created_by, claimed_by, claimed_at, created_at, expires_at
    FROM host_invitations
    ORDER BY created_at DESC
  |]

-- | Get all host invitations with creator and claimer display names.
--
-- Joins with @user_metadata@ to resolve user IDs to display names for
-- the admin list view.
getAllWithCreator :: Hasql.Statement () [ModelWithCreator]
getAllWithCreator =
  interp
    True
    [sql|
    SELECT
      hi.id,
      hi.token,
      hi.status,
      hi.schedule_data,
      hi.recipient_email,
      creator_um.display_name,
      claimer_um.display_name,
      hi.created_at,
      hi.expires_at
    FROM host_invitations hi
    INNER JOIN user_metadata creator_um ON hi.created_by = creator_um.user_id
    LEFT JOIN user_metadata claimer_um ON hi.claimed_by = claimer_um.user_id
    ORDER BY hi.created_at DESC
  |]

-- | Claim a host invitation, marking it as claimed.
--
-- This atomically:
-- 1. Marks the invitation as 'claimed'
-- 2. Sets the claimed_by user ID
-- 3. Sets the claimed_at timestamp
--
-- Returns the model if successful, Nothing if:
-- - Token doesn't exist
-- - Invitation is not in 'pending' status
-- - Invitation has expired
claimInvitation :: Token -> User.Id -> Hasql.Statement () (Maybe Model)
claimInvitation tokenValue userId =
  interp
    False
    [sql|
    UPDATE host_invitations
    SET status = 'claimed',
        claimed_by = #{userId},
        claimed_at = NOW()
    WHERE token = #{tokenValue}
      AND status = 'pending'
      AND expires_at > NOW()
    RETURNING id, token, status, schedule_data, recipient_email, created_by, claimed_by, claimed_at, created_at, expires_at
  |]

-- | Revoke a pending host invitation.
--
-- Returns the model if successful, Nothing if:
-- - Invitation doesn't exist
-- - Invitation is not in 'pending' status
revokeInvitation :: Id -> Hasql.Statement () (Maybe Model)
revokeInvitation invitationId =
  interp
    False
    [sql|
    UPDATE host_invitations
    SET status = 'revoked'
    WHERE id = #{invitationId}
      AND status = 'pending'
    RETURNING id, token, status, schedule_data, recipient_email, created_by, claimed_by, claimed_at, created_at, expires_at
  |]

-- | Get a host invitation by its ID.
getById :: Id -> Hasql.Statement () (Maybe Model)
getById invitationId =
  interp
    False
    [sql|
    SELECT id, token, status, schedule_data, recipient_email, created_by, claimed_by, claimed_at, created_at, expires_at
    FROM host_invitations
    WHERE id = #{invitationId}
  |]

--------------------------------------------------------------------------------

-- | Update the recipient email of a pending invitation.
--
-- Returns the updated model if the invitation exists and is in 'pending'
-- status. Returns Nothing otherwise.
updateRecipientEmail :: Id -> EmailAddress -> Hasql.Statement () (Maybe Model)
updateRecipientEmail invitationId newEmail =
  interp
    False
    [sql|
    UPDATE host_invitations
    SET recipient_email = #{newEmail}
    WHERE id = #{invitationId}
      AND status = 'pending'
    RETURNING id, token, status, schedule_data, recipient_email,
              created_by, claimed_by, claimed_at, created_at, expires_at
  |]

--------------------------------------------------------------------------------
-- Token generation

-- | Crockford-style base32 alphabet — excludes @0@, @O@, @1@, @I@, @L@
--   to avoid visual ambiguity when read aloud or transcribed.
inviteCodeAlphabet :: Text
inviteCodeAlphabet = "23456789ABCDEFGHJKMNPQRSTVWXYZ"

-- | Generate a human-readable invitation code of the form
--   @INV-XXXX-XXXX@ where each @X@ is drawn from 'inviteCodeAlphabet'.
--
--   Entropy: @30^8 ≈ 6.5×10^11@ combinations. Sufficient against
--   rate-limited brute-force on 7-day-expiring single-use tokens;
--   not appropriate for non-expiring secrets.
--
--   Uses 'System.Random.randomRIO' on the 'IO' RNG. If a future
--   threat model demands stronger guarantees, swap to a CSPRNG —
--   the signature does not change.
generateInviteCode :: IO Token
generateInviteCode = do
  let alpha = Text.unpack inviteCodeAlphabet
      n = length alpha
      pick = do
        idx <- Random.randomRIO (0, n - 1)
        pure (alpha !! idx)
  g1 <- replicateM 4 pick
  g2 <- replicateM 4 pick
  pure $ Token (Text.pack ("INV-" <> g1 <> "-" <> g2))
