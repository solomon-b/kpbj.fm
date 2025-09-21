{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Database.Tables.UserMetadata where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue, EncodeRow, EncodeValue, OneRow, interp, sql)
import Hasql.Statement qualified as Hasql
import Servant qualified

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
    mIsAdmin :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

-- | API Domain Type for @UserMetadata@.
data Domain = Domain
  { dId :: Id,
    dUserId :: User.Id,
    dDisplayName :: DisplayName,
    dFullName :: FullName,
    dAvatarUrl :: Maybe Text,
    dIsAdmin :: Bool
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance Domain)
  deriving anyclass (FromJSON, ToJSON)

toDomain :: Model -> Domain
toDomain Model {..} =
  Domain
    { dId = mId,
      dUserId = mUserId,
      dDisplayName = mDisplayName,
      dFullName = mFullName,
      dAvatarUrl = mAvatarUrl,
      dIsAdmin = mIsAdmin
    }

--------------------------------------------------------------------------------

getUserMetadata :: User.Id -> Hasql.Statement () (Maybe Model)
getUserMetadata userId =
  interp
    False
    [sql|
    SELECT id, user_id, display_name, full_name, avatar_url, is_admin
    FROM user_metadata
    WHERE user_id = #{userId}
  |]

data ModelInsert = ModelInsert
  { miUserId :: User.Id,
    miDisplayName :: DisplayName,
    miFullName :: FullName,
    miAvatarUrl :: Maybe Text,
    miIsAdmin :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelInsert
  deriving (Display) via (RecordInstance ModelInsert)

insertUserMetadata :: ModelInsert -> Hasql.Statement () (OneRow Id)
insertUserMetadata ModelInsert {..} =
  interp
    False
    [sql|
    INSERT INTO user_metadata(user_id, display_name, full_name, avatar_url, is_admin, created_at, updated_at)
    VALUES (#{miUserId}, #{miDisplayName}, #{miFullName}, #{miAvatarUrl}, #{miIsAdmin}, NOW(), NOW())
    RETURNING id
  |]

-- | Model for inserting both user and metadata in a single transaction
data UserWithMetadataInsert = UserWithMetadataInsert
  { uwmiEmail :: EmailAddress,
    uwmiPassword :: PasswordHash Argon2,
    uwmiDisplayName :: DisplayName,
    uwmiFullName :: FullName,
    uwmiAvatarUrl :: Maybe Text,
    uwmiIsAdmin :: Bool
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
      INSERT INTO user_metadata(user_id, display_name, full_name, avatar_url, is_admin, created_at, updated_at)
      SELECT id, #{uwmiDisplayName}, #{uwmiFullName}, #{uwmiAvatarUrl}, #{uwmiIsAdmin}, NOW(), NOW()
      FROM new_user
      RETURNING user_id
    )
    SELECT id FROM new_user
  |]
