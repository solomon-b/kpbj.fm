{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Database.Tables.UserMetadata where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..), display, displayBuilder)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneRow, interp, sql)
import Hasql.Statement qualified as Hasql
import Servant qualified

--------------------------------------------------------------------------------
-- UserRole Type

data UserRole = User | Host | Staff | Admin
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance Display UserRole where
  displayBuilder User = "User"
  displayBuilder Host = "Host"
  displayBuilder Staff = "Staff"
  displayBuilder Admin = "Admin"

instance DecodeValue UserRole where
  decodeValue = Decoders.enum $ \case
    "User" -> Just User
    "Host" -> Just Host
    "Staff" -> Just Staff
    "Admin" -> Just Admin
    _ -> Nothing

instance EncodeValue UserRole where
  encodeValue = Encoders.enum $ \case
    User -> "User"
    Host -> "Host"
    Staff -> "Staff"
    Admin -> "Admin"

instance Servant.FromHttpApiData UserRole where
  parseUrlPiece "User" = Right User
  parseUrlPiece "Host" = Right Host
  parseUrlPiece "Staff" = Right Staff
  parseUrlPiece "Admin" = Right Admin
  parseUrlPiece invalid = Left $ "Invalid UserRole: " <> invalid

instance Servant.ToHttpApiData UserRole where
  toUrlPiece = display

-- | Check if a user role has administrative privileges
isAdmin :: UserRole -> Bool
isAdmin Admin = True
isAdmin _ = False

-- | Check if a user role has staff-level privileges or higher
isStaffOrHigher :: UserRole -> Bool
isStaffOrHigher Staff = True
isStaffOrHigher Admin = True
isStaffOrHigher _ = False

-- | Check if a user role has host-level privileges or higher
isHostOrHigher :: UserRole -> Bool
isHostOrHigher Host = True
isHostOrHigher Staff = True
isHostOrHigher Admin = True
isHostOrHigher _ = False

--------------------------------------------------------------------------------
-- Model

newtype Id = Id Int64
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      ToJSON,
      FromJSON,
      Display,
      DecodeValue,
      EncodeValue
    )

-- | Database Model for the @user_metadata@ table.
data Model = Model
  { mId :: Id,
    mUserId :: User.Id,
    mDisplayName :: DisplayName,
    mFullName :: FullName,
    mAvatarUrl :: Maybe Text,
    mUserRole :: UserRole
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

--------------------------------------------------------------------------------

getUserMetadata :: User.Id -> Hasql.Statement () (Maybe Model)
getUserMetadata userId =
  interp
    False
    [sql|
    SELECT id, user_id, display_name, full_name, avatar_url, user_role
    FROM user_metadata
    WHERE user_id = #{userId}
  |]

data ModelInsert = ModelInsert
  { miUserId :: User.Id,
    miDisplayName :: DisplayName,
    miFullName :: FullName,
    miAvatarUrl :: Maybe Text,
    miUserRole :: UserRole
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelInsert
  deriving (Display) via (RecordInstance ModelInsert)

insertUserMetadata :: ModelInsert -> Hasql.Statement () (OneRow Id)
insertUserMetadata ModelInsert {..} =
  interp
    False
    [sql|
    INSERT INTO user_metadata(user_id, display_name, full_name, avatar_url, user_role, created_at, updated_at)
    VALUES (#{miUserId}, #{miDisplayName}, #{miFullName}, #{miAvatarUrl}, #{miUserRole}, NOW(), NOW())
    RETURNING id
  |]

-- | Model for inserting both user and metadata in a single transaction
data UserWithMetadataInsert = UserWithMetadataInsert
  { uwmiEmail :: EmailAddress,
    uwmiPassword :: PasswordHash Argon2,
    uwmiDisplayName :: DisplayName,
    uwmiFullName :: FullName,
    uwmiAvatarUrl :: Maybe Text,
    uwmiUserRole :: UserRole
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via UserWithMetadataInsert
  deriving (Display) via (RecordInstance UserWithMetadataInsert)

-- | Insert user and metadata in a single atomic transaction
insertUserWithMetadata :: UserWithMetadataInsert -> Hasql.Statement () (OneRow User.Id)
insertUserWithMetadata UserWithMetadataInsert {..} =
  interp
    False
    [sql|
    WITH new_user AS (
      INSERT INTO users(email, password, created_at, updated_at)
      VALUES (#{uwmiEmail}, #{uwmiPassword}, NOW(), NOW())
      RETURNING id
    )
    , new_metadata AS (
      INSERT INTO user_metadata(user_id, display_name, full_name, avatar_url, user_role, created_at, updated_at)
      SELECT id, #{uwmiDisplayName}, #{uwmiFullName}, #{uwmiAvatarUrl}, #{uwmiUserRole}, NOW(), NOW()
      FROM new_user
      RETURNING user_id
    )
    SELECT id FROM new_user
  |]
