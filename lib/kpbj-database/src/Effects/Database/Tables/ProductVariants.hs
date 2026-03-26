{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Database table definition and queries for @product_variants@.
--
-- Uses rel8 for simple queries.
module Effects.Database.Tables.ProductVariants
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    ProductVariant (..),
    productVariantSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getByProductId,
    getById,
    insertVariant,
    updateVariant,
    softDeleteVariant,
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
import Domain.Types.Cents (Cents)
import Effects.Database.Tables.Products qualified as Products
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

-- | Newtype wrapper for product variant primary keys.
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

-- | The @product_variants@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data ProductVariant f = ProductVariant
  { pvId :: Column f Id,
    pvProductId :: Column f Products.Id,
    pvLabel :: Column f Text,
    pvPriceCents :: Column f (Maybe Cents),
    pvInventoryCount :: Column f Int64,
    pvSku :: Column f (Maybe Text),
    pvWeightOz :: Column f (Maybe Int64),
    pvSortOrder :: Column f Int64,
    pvCreatedAt :: Column f UTCTime,
    pvDeletedAt :: Column f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (ProductVariant f)

deriving stock instance (f ~ Result) => Eq (ProductVariant f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (ProductVariant Result)


-- | Display instance for ProductVariant Result.
instance Display (ProductVariant Result) where
  displayBuilder variant =
    "ProductVariant { id = "
      <> displayBuilder variant.pvId
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @ProductVariant Result@.
type Model = ProductVariant Result

-- | Table schema connecting the Haskell type to the database table.
productVariantSchema :: TableSchema (ProductVariant Name)
productVariantSchema =
  TableSchema
    { name = "product_variants",
      columns =
        ProductVariant
          { pvId = "id",
            pvProductId = "product_id",
            pvLabel = "label",
            pvPriceCents = "price_cents",
            pvInventoryCount = "inventory_count",
            pvSku = "sku",
            pvWeightOz = "weight_oz",
            pvSortOrder = "sort_order",
            pvCreatedAt = "created_at",
            pvDeletedAt = "deleted_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating or updating product variants.
data Insert = Insert
  { viProductId :: Products.Id,
    viLabel :: Text,
    viPriceCents :: Maybe Cents,
    viInventoryCount :: Int64,
    viSku :: Maybe Text,
    viWeightOz :: Maybe Int64,
    viSortOrder :: Int64
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Get all variants for a product, ordered by sort_order.
getByProductId :: Products.Id -> Hasql.Statement () [Model]
getByProductId productId =
  run $
    select $
      orderBy (pvSortOrder >$< asc) do
        variant <- each productVariantSchema
        where_ $ pvProductId variant ==. lit productId
        where_ $ isNull (pvDeletedAt variant)
        pure variant

-- | Get a product variant by ID.
getById :: Id -> Hasql.Statement () (Maybe Model)
getById variantId = fmap listToMaybe $ run $ select do
  variant <- each productVariantSchema
  where_ $ pvId variant ==. lit variantId
  pure variant

-- | Insert a new product variant.
insertVariant :: Insert -> Hasql.Statement () (Maybe Id)
insertVariant Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = productVariantSchema,
            rows =
              values
                [ ProductVariant
                    { pvId = nextId "product_variants_id_seq",
                      pvProductId = lit viProductId,
                      pvLabel = lit viLabel,
                      pvPriceCents = lit viPriceCents,
                      pvInventoryCount = lit viInventoryCount,
                      pvSku = lit viSku,
                      pvWeightOz = lit viWeightOz,
                      pvSortOrder = lit viSortOrder,
                      pvCreatedAt = now,
                      pvDeletedAt = lit Nothing
                    }
                ],
            onConflict = Abort,
            returning = Returning pvId
          }

-- | Update an existing product variant.
updateVariant :: Id -> Insert -> Hasql.Statement () (Maybe Id)
updateVariant variantId Insert {..} =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = productVariantSchema,
            from = pure (),
            set = \_ variant ->
              variant
                { pvLabel = lit viLabel,
                  pvPriceCents = lit viPriceCents,
                  pvInventoryCount = lit viInventoryCount,
                  pvSku = lit viSku,
                  pvWeightOz = lit viWeightOz,
                  pvSortOrder = lit viSortOrder
                },
            updateWhere = \_ variant -> pvId variant ==. lit variantId,
            returning = Returning pvId
          }

-- | Soft-delete a variant by setting deleted_at.
softDeleteVariant :: Id -> Hasql.Statement () (Maybe Id)
softDeleteVariant variantId =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = productVariantSchema,
            from = pure (),
            set = \_ variant ->
              variant { pvDeletedAt = nullify now },
            updateWhere = \_ variant -> pvId variant ==. lit variantId,
            returning = Returning pvId
          }
