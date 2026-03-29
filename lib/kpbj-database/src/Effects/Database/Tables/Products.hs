{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Database table definition and queries for @products@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
module Effects.Database.Tables.Products
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    Product (..),
    productSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getAll,
    getById,
    getBySlug,
    insertProduct,
    updateProduct,
    deactivateProduct,

    -- * Listing Types
    ProductWithHeroImage (..),
    pwhToProduct,

    -- * Listing Queries
    getActiveWithHeroImage,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Domain.Types.Cents (Cents)
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for product primary keys.
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

-- | The @products@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data Product f = Product
  { pId :: Column f Id,
    pName :: Column f Text,
    pSlug :: Column f Text,
    pDescription :: Column f Text,
    pBasePriceCents :: Column f Cents,
    pWeightOz :: Column f Int64,
    pCategory :: Column f (Maybe Text),
    pInventoryCount :: Column f Int64,
    pIsActive :: Column f Bool,
    pSortOrder :: Column f Int64,
    pCreatedAt :: Column f UTCTime,
    pUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (Product f)

deriving stock instance (f ~ Result) => Eq (Product f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (Product Result)


-- | Display instance for Product Result.
instance Display (Product Result) where
  displayBuilder p =
    "Product { id = "
      <> displayBuilder p.pId
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @Product Result@.
type Model = Product Result

-- | Table schema connecting the Haskell type to the database table.
productSchema :: TableSchema (Product Name)
productSchema =
  TableSchema
    { name = "products",
      columns =
        Product
          { pId = "id",
            pName = "name",
            pSlug = "slug",
            pDescription = "description",
            pBasePriceCents = "base_price_cents",
            pWeightOz = "weight_oz",
            pCategory = "category",
            pInventoryCount = "inventory_count",
            pIsActive = "is_active",
            pSortOrder = "sort_order",
            pCreatedAt = "created_at",
            pUpdatedAt = "updated_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new products.
data Insert = Insert
  { piName :: Text,
    piSlug :: Text,
    piDescription :: Text,
    piBasePriceCents :: Cents,
    piWeightOz :: Int64,
    piCategory :: Maybe Text,
    piInventoryCount :: Int64,
    piIsActive :: Bool,
    piSortOrder :: Int64
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Get all products ordered by sort_order.
--
-- Uses the @products_with_inventory@ view which replaces product-level
-- inventory with the sum of active variant inventories when variants exist.
getAll :: Hasql.Statement () [Model]
getAll = interp False
  [sql|
    SELECT id, name, slug, description, base_price_cents,
           weight_oz, category, inventory_count,
           is_active, sort_order, created_at, updated_at
    FROM products_with_inventory
    ORDER BY sort_order ASC
  |]

-- | Get product by ID.
--
-- Uses the @products_with_inventory@ view for effective inventory.
getById :: Id -> Hasql.Statement () (Maybe Model)
getById productId = interp False
  [sql|
    SELECT id, name, slug, description, base_price_cents,
           weight_oz, category, inventory_count,
           is_active, sort_order, created_at, updated_at
    FROM products_with_inventory
    WHERE id = #{productId}
  |]

-- | Get product by slug.
--
-- Uses the @products_with_inventory@ view for effective inventory.
getBySlug :: Text -> Hasql.Statement () (Maybe Model)
getBySlug slug = interp False
  [sql|
    SELECT id, name, slug, description, base_price_cents,
           weight_oz, category, inventory_count,
           is_active, sort_order, created_at, updated_at
    FROM products_with_inventory
    WHERE slug = #{slug}
  |]

-- | Insert a new product.
insertProduct :: Insert -> Hasql.Statement () (Maybe Id)
insertProduct Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = productSchema,
            rows =
              values
                [ Product
                    { pId = nextId "products_id_seq",
                      pName = lit piName,
                      pSlug = lit piSlug,
                      pDescription = lit piDescription,
                      pBasePriceCents = lit piBasePriceCents,
                      pWeightOz = lit piWeightOz,
                      pCategory = lit piCategory,
                      pInventoryCount = lit piInventoryCount,
                      pIsActive = lit piIsActive,
                      pSortOrder = lit piSortOrder,
                      pCreatedAt = now,
                      pUpdatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning pId
          }

-- | Update a product.
updateProduct :: Id -> Insert -> Hasql.Statement () (Maybe Id)
updateProduct productId Insert {..} =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = productSchema,
            from = pure (),
            set = \_ p ->
              p
                { pName = lit piName,
                  pSlug = lit piSlug,
                  pDescription = lit piDescription,
                  pBasePriceCents = lit piBasePriceCents,
                  pWeightOz = lit piWeightOz,
                  pCategory = lit piCategory,
                  pInventoryCount = lit piInventoryCount,
                  pIsActive = lit piIsActive,
                  pSortOrder = lit piSortOrder,
                  pUpdatedAt = now
                },
            updateWhere = \_ p -> pId p ==. lit productId,
            returning = Returning pId
          }

-- | Deactivate a product by setting is_active to false.
deactivateProduct :: Id -> Hasql.Statement () (Maybe Id)
deactivateProduct productId =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = productSchema,
            from = pure (),
            set = \_ p ->
              p
                { pIsActive = lit False,
                  pUpdatedAt = now
                },
            updateWhere = \_ p -> pId p ==. lit productId,
            returning = Returning pId
          }


--------------------------------------------------------------------------------
-- Listing Types

-- | A product with its hero (first) image for listing pages.
--
-- This is a flat record rather than nesting 'Model' because the generic
-- 'DecodeRow' derivation requires every field to be a single-column
-- 'DecodeValue'. Use 'pwhToProduct' to extract the 'Model'.
data ProductWithHeroImage = ProductWithHeroImage
  { pwhId :: Id,
    pwhName :: Text,
    pwhSlug :: Text,
    pwhDescription :: Text,
    pwhBasePriceCents :: Cents,
    pwhWeightOz :: Int64,
    pwhCategory :: Maybe Text,
    pwhInventoryCount :: Int64,
    pwhIsActive :: Bool,
    pwhSortOrder :: Int64,
    pwhCreatedAt :: UTCTime,
    pwhUpdatedAt :: UTCTime,
    pwhHeroImagePath :: Maybe Text,
    pwhHeroAltText :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)


-- | Display instance for ProductWithHeroImage.
instance Display ProductWithHeroImage where
  displayBuilder pwh =
    "ProductWithHeroImage { id = "
      <> displayBuilder pwh.pwhId
      <> " }"


-- | Extract the 'Model' from a 'ProductWithHeroImage'.
pwhToProduct :: ProductWithHeroImage -> Model
pwhToProduct ProductWithHeroImage {..} =
  Product
    { pId = pwhId,
      pName = pwhName,
      pSlug = pwhSlug,
      pDescription = pwhDescription,
      pBasePriceCents = pwhBasePriceCents,
      pWeightOz = pwhWeightOz,
      pCategory = pwhCategory,
      pInventoryCount = pwhInventoryCount,
      pIsActive = pwhIsActive,
      pSortOrder = pwhSortOrder,
      pCreatedAt = pwhCreatedAt,
      pUpdatedAt = pwhUpdatedAt
    }


--------------------------------------------------------------------------------
-- Listing Queries

-- | Get all active products with their hero image (first by sort_order).
--
-- Uses the @products_with_inventory@ view for effective inventory.
-- Returns all active products, including out-of-stock ones.
getActiveWithHeroImage :: Hasql.Statement () [ProductWithHeroImage]
getActiveWithHeroImage = interp False
  [sql|
    SELECT p.id, p.name, p.slug, p.description, p.base_price_cents,
           p.weight_oz, p.category, p.inventory_count,
           p.is_active, p.sort_order, p.created_at, p.updated_at,
           pi.image_path, pi.alt_text
    FROM products_with_inventory p
    LEFT JOIN LATERAL (
      SELECT image_path, alt_text
      FROM product_images
      WHERE product_id = p.id
      ORDER BY sort_order, id
      LIMIT 1
    ) pi ON true
    WHERE p.is_active = true
    ORDER BY p.sort_order, p.id
  |]
