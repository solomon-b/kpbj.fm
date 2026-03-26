{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Database table definition and queries for @product_option_values@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for cross-table joins.
module Effects.Database.Tables.ProductOptionValues
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    ProductOptionValue (..),
    productOptionValueSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getByOptionTypeId,
    getByProductId,
    insertOptionValue,
    deleteOptionValue,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Effects.Database.Tables.ProductOptionTypes qualified as ProductOptionTypes
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Insert)
import Rel8 qualified
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for product option value primary keys.
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

-- | The @product_option_values@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data ProductOptionValue f = ProductOptionValue
  { povId :: Column f Id,
    povOptionTypeId :: Column f ProductOptionTypes.Id,
    povValue :: Column f Text,
    povSortOrder :: Column f Int64
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (ProductOptionValue f)

deriving stock instance (f ~ Result) => Eq (ProductOptionValue f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (ProductOptionValue Result)


-- | Display instance for ProductOptionValue Result.
instance Display (ProductOptionValue Result) where
  displayBuilder optionValue =
    "ProductOptionValue { id = "
      <> displayBuilder optionValue.povId
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @ProductOptionValue Result@.
type Model = ProductOptionValue Result

-- | Table schema connecting the Haskell type to the database table.
productOptionValueSchema :: TableSchema (ProductOptionValue Name)
productOptionValueSchema =
  TableSchema
    { name = "product_option_values",
      columns =
        ProductOptionValue
          { povId = "id",
            povOptionTypeId = "option_type_id",
            povValue = "value",
            povSortOrder = "sort_order"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new product option values.
data Insert = Insert
  { oviOptionTypeId :: ProductOptionTypes.Id,
    oviValue :: Text,
    oviSortOrder :: Int64
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Get all option values for a given option type, ordered by sort_order.
getByOptionTypeId :: ProductOptionTypes.Id -> Hasql.Statement () [Model]
getByOptionTypeId optionTypeId =
  run $
    select $
      orderBy (povSortOrder >$< asc) do
        value <- each productOptionValueSchema
        where_ $ povOptionTypeId value ==. lit optionTypeId
        pure value

-- | Get all option values for a product, joining through option_types.
--
-- Results are ordered by option type sort_order then option value sort_order.
getByProductId :: Products.Id -> Hasql.Statement () [Model]
getByProductId productId =
  interp
    False
    [sql|
      SELECT pov.id, pov.option_type_id, pov.value, pov.sort_order
      FROM product_option_values pov
      JOIN product_option_types pot ON pot.id = pov.option_type_id
      WHERE pot.product_id = #{productId}
      ORDER BY pot.sort_order, pov.sort_order
    |]

-- | Insert a new product option value.
insertOptionValue :: Insert -> Hasql.Statement () Id
insertOptionValue Insert {..} =
  run1 $
    insert
        Rel8.Insert
          { into = productOptionValueSchema,
            rows =
              values
                [ ProductOptionValue
                    { povId = nextId "product_option_values_id_seq",
                      povOptionTypeId = lit oviOptionTypeId,
                      povValue = lit oviValue,
                      povSortOrder = lit oviSortOrder
                    }
                ],
            onConflict = Abort,
            returning = Returning povId
          }

-- | Delete a product option value by ID.
deleteOptionValue :: Id -> Hasql.Statement () (Maybe Id)
deleteOptionValue optionValueId =
  fmap listToMaybe $
    run $
      delete
        Rel8.Delete
          { from = productOptionValueSchema,
            using = pure (),
            deleteWhere = \_ value -> povId value ==. lit optionValueId,
            returning = Returning povId
          }
