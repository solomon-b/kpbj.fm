{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Database table definition and queries for @product_option_types@.
--
-- Uses rel8 for simple queries.
module Effects.Database.Tables.ProductOptionTypes
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    ProductOptionType (..),
    productOptionTypeSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getByProductId,
    insertOptionType,
    deleteOptionType,
    deleteByProductId,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..))
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Insert)
import Rel8 qualified
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for product option type primary keys.
--
-- Provides type safety to prevent mixing up IDs from different tables.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq, DBOrd)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Table Definition

-- | The @product_option_types@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data ProductOptionType f = ProductOptionType
  { potId :: Column f Id,
    potProductId :: Column f Products.Id,
    potName :: Column f Text,
    potSortOrder :: Column f Int64
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (ProductOptionType f)

deriving stock instance (f ~ Result) => Eq (ProductOptionType f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (ProductOptionType Result)


-- | Display instance for ProductOptionType Result.
instance Display (ProductOptionType Result) where
  displayBuilder optionType =
    "ProductOptionType { id = "
      <> displayBuilder optionType.potId
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @ProductOptionType Result@.
type Model = ProductOptionType Result

-- | Table schema connecting the Haskell type to the database table.
productOptionTypeSchema :: TableSchema (ProductOptionType Name)
productOptionTypeSchema =
  TableSchema
    { name = "product_option_types",
      columns =
        ProductOptionType
          { potId = "id",
            potProductId = "product_id",
            potName = "name",
            potSortOrder = "sort_order"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new product option types.
data Insert = Insert
  { otiProductId :: Products.Id,
    otiName :: Text,
    otiSortOrder :: Int64
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Get all option types for a product, ordered by sort_order.
getByProductId :: Products.Id -> Hasql.Statement () [Model]
getByProductId productId =
  run $
    select $
      orderBy (potSortOrder >$< asc) do
        optionType <- each productOptionTypeSchema
        where_ $ potProductId optionType ==. lit productId
        pure optionType

-- | Insert a new product option type.
insertOptionType :: Insert -> Hasql.Statement () Id
insertOptionType Insert {..} =
  run1 $
    insert
        Rel8.Insert
          { into = productOptionTypeSchema,
            rows =
              values
                [ ProductOptionType
                    { potId = nextId "product_option_types_id_seq",
                      potProductId = lit otiProductId,
                      potName = lit otiName,
                      potSortOrder = lit otiSortOrder
                    }
                ],
            onConflict = Abort,
            returning = Returning potId
          }

-- | Delete a product option type by ID.
--
-- CASCADE deletes associated option values.
deleteOptionType :: Id -> Hasql.Statement () (Maybe Id)
deleteOptionType optionTypeId =
  fmap listToMaybe $
    run $
      delete
        Rel8.Delete
          { from = productOptionTypeSchema,
            using = pure (),
            deleteWhere = \_ optionType -> potId optionType ==. lit optionTypeId,
            returning = Returning potId
          }

-- | Delete all option types for a product.
--
-- CASCADE deletes associated option values and variant-option join rows.
deleteByProductId :: Products.Id -> Hasql.Statement () [Id]
deleteByProductId productId =
  run $
    delete
      Rel8.Delete
        { from = productOptionTypeSchema,
          using = pure (),
          deleteWhere = \_ optionType -> potProductId optionType ==. lit productId,
          returning = Returning potId
        }
