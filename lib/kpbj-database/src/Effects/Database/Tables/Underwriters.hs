{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @underwriters@.
--
-- An underwriter is a business that supports the station. One underwriter can
-- have several break items, and the planner gives it one daily share.
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
    insertUnderwriter,
    renameUnderwriter,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..))
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for underwriter primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq, DBOrd)
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
  run $
    select $
      orderBy ((uwName >$< asc) <> (uwId >$< asc)) $
        each underwriterSchema

-- | Insert an underwriter and return its ID.
insertUnderwriter :: Text -> Hasql.Statement () (Maybe Id)
insertUnderwriter newName =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = underwriterSchema,
            rows =
              values
                [ Underwriter
                    { uwId = nextId "underwriters_id_seq",
                      uwName = lit newName,
                      uwCreatedAt = now,
                      uwUpdatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning uwId
          }

-- | Rename an underwriter. Returns Nothing when the ID names no row.
renameUnderwriter :: Id -> Text -> Hasql.Statement () (Maybe Model)
renameUnderwriter underwriterId newName =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = underwriterSchema,
            from = pure (),
            set = \_ row -> row {uwName = lit newName, uwUpdatedAt = now},
            updateWhere = \_ row -> uwId row ==. lit underwriterId,
            returning = Returning id
          }
