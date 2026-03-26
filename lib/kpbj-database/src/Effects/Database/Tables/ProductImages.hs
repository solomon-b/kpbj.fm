{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Database table definition and queries for @product_images@.
--
-- Uses rel8 for simple queries.
module Effects.Database.Tables.ProductImages
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    ProductImage (..),
    productImageSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getById,
    getByProductId,
    insertImage,
    deleteImage,
    updateSortOrder,
    updateImageMeta,
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

-- | Newtype wrapper for product image primary keys.
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

-- | The @product_images@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data ProductImage f = ProductImage
  { piId :: Column f Id,
    piProductId :: Column f Products.Id,
    piImagePath :: Column f Text,
    piAltText :: Column f Text,
    piSortOrder :: Column f Int64,
    piCreatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (ProductImage f)

deriving stock instance (f ~ Result) => Eq (ProductImage f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (ProductImage Result)


-- | Display instance for ProductImage Result.
instance Display (ProductImage Result) where
  displayBuilder image =
    "ProductImage { id = "
      <> displayBuilder image.piId
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @ProductImage Result@.
type Model = ProductImage Result

-- | Table schema connecting the Haskell type to the database table.
productImageSchema :: TableSchema (ProductImage Name)
productImageSchema =
  TableSchema
    { name = "product_images",
      columns =
        ProductImage
          { piId = "id",
            piProductId = "product_id",
            piImagePath = "image_path",
            piAltText = "alt_text",
            piSortOrder = "sort_order",
            piCreatedAt = "created_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new product images.
data Insert = Insert
  { iiProductId :: Products.Id,
    iiImagePath :: Text,
    iiAltText :: Text,
    iiSortOrder :: Int64
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Get all images for a product, ordered by sort_order.
getByProductId :: Products.Id -> Hasql.Statement () [Model]
getByProductId productId =
  run $
    select $
      orderBy (piSortOrder >$< asc) do
        image <- each productImageSchema
        where_ $ piProductId image ==. lit productId
        pure image

-- | Insert a new product image.
insertImage :: Insert -> Hasql.Statement () (Maybe Id)
insertImage Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = productImageSchema,
            rows =
              values
                [ ProductImage
                    { piId = nextId "product_images_id_seq",
                      piProductId = lit iiProductId,
                      piImagePath = lit iiImagePath,
                      piAltText = lit iiAltText,
                      piSortOrder = lit iiSortOrder,
                      piCreatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning piId
          }

-- | Delete a product image by ID.
deleteImage :: Id -> Hasql.Statement () (Maybe Id)
deleteImage imageId =
  fmap listToMaybe $
    run $
      delete
        Rel8.Delete
          { from = productImageSchema,
            using = pure (),
            deleteWhere = \_ image -> piId image ==. lit imageId,
            returning = Returning piId
          }

-- | Get a single product image by its ID.
getById :: Id -> Hasql.Statement () (Maybe Model)
getById imageId =
  fmap listToMaybe $
    run $
      select do
        image <- each productImageSchema
        where_ $ piId image ==. lit imageId
        pure image


-- | Update the sort order of a product image.
updateSortOrder :: Id -> Int64 -> Hasql.Statement () ()
updateSortOrder imageId newSortOrder =
  run_ $
    update
      Rel8.Update
        { target = productImageSchema,
          from = pure (),
          set = \_ image -> image {piSortOrder = lit newSortOrder},
          updateWhere = \_ image -> piId image ==. lit imageId,
          returning = NoReturning
        }


-- | Update both sort order and alt text of a product image.
updateImageMeta ::
  Id ->
  -- | New sort order.
  Int64 ->
  -- | New alt text.
  Text ->
  Hasql.Statement () ()
updateImageMeta imageId newSortOrder newAltText =
  run_ $
    update
      Rel8.Update
        { target = productImageSchema,
          from = pure (),
          set = \_ image ->
            image
              { piSortOrder = lit newSortOrder,
                piAltText = lit newAltText
              },
          updateWhere = \_ image -> piId image ==. lit imageId,
          returning = NoReturning
        }
