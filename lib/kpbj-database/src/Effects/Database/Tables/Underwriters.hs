{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @underwriters@.
--
-- An underwriter is a business that pays for underwriting announcements. One
-- underwriter can have several break items, and the planner spaces them as one.
module Effects.Database.Tables.Underwriters
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    Underwriter (..),
    underwriterSchema,

    -- * Model (Result alias)
    Model,

    -- * Queries
    getAll,
    getById,
    insertUnderwriter,
    renameUnderwriter,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Insert)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for underwriter primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Table Definition

-- | The @underwriters@ table definition using rel8's higher-kinded data pattern.
data Underwriter f = Underwriter
  { uwId :: Column f Id,
    uwName :: Column f Text,
    uwCreatedAt :: Column f UTCTime,
    uwUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (Underwriter f)

deriving stock instance (f ~ Result) => Eq (Underwriter f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (Underwriter Result)

-- | Display instance for Underwriter Result.
instance Display (Underwriter Result) where
  displayBuilder m = displayBuilder (uwId m) <> " - " <> displayBuilder (uwName m)

-- | @Model@ is the same as @Underwriter Result@.
type Model = Underwriter Result

-- | Table schema connecting the Haskell type to the database table.
underwriterSchema :: TableSchema (Underwriter Name)
underwriterSchema =
  TableSchema
    { name = "underwriters",
      columns =
        Underwriter
          { uwId = "id",
            uwName = "name",
            uwCreatedAt = "created_at",
            uwUpdatedAt = "updated_at"
          }
    }

--------------------------------------------------------------------------------
-- Queries

-- | Every underwriter, by name.
getAll :: Hasql.Statement () [Model]
getAll =
  interp
    False
    [sql|
    SELECT id, name, created_at, updated_at
    FROM underwriters
    ORDER BY name, id
  |]

-- | Get an underwriter by its ID.
getById :: Id -> Hasql.Statement () (Maybe Model)
getById underwriterId =
  listToMaybe
    <$> interp
      False
      [sql|
      SELECT id, name, created_at, updated_at
      FROM underwriters
      WHERE id = #{underwriterId}
    |]

-- | Insert an underwriter and return its ID.
insertUnderwriter :: Text -> Hasql.Statement () (Maybe Id)
insertUnderwriter newName =
  listToMaybe
    <$> interp
      False
      [sql|
      INSERT INTO underwriters (name) VALUES (#{newName})
      RETURNING id
    |]

-- | Rename an underwriter. Returns Nothing when the ID names no row.
renameUnderwriter :: Id -> Text -> Hasql.Statement () (Maybe Model)
renameUnderwriter underwriterId newName =
  listToMaybe
    <$> interp
      False
      [sql|
      UPDATE underwriters
      SET name = #{newName}, updated_at = NOW()
      WHERE id = #{underwriterId}
      RETURNING id, name, created_at, updated_at
    |]
