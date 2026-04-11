{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Database table definition and queries for @order_items@.
module Effects.Database.Tables.OrderItems
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    OrderItem (..),
    orderItemSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    insertOrderItem,
    getByOrderId,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Domain.Types.Cents (Cents)
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Insert)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for order item primary keys.
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

-- | The @order_items@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data OrderItem f = OrderItem
  { oiId :: Column f Id,
    oiOrderId :: Column f Orders.Id,
    oiProductId :: Column f Products.Id,
    oiVariantId :: Column f (Maybe ProductVariants.Id),
    oiProductName :: Column f Text,
    oiVariantLabel :: Column f Text,
    oiQuantity :: Column f Int64,
    oiUnitPriceCents :: Column f Cents,
    oiCreatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (OrderItem f)

deriving stock instance (f ~ Result) => Eq (OrderItem f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (OrderItem Result)

-- | Display instance for OrderItem Result.
instance Display (OrderItem Result) where
  displayBuilder oi =
    "OrderItem { id = "
      <> displayBuilder oi.oiId
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @OrderItem Result@.
type Model = OrderItem Result

-- | Table schema connecting the Haskell type to the database table.
orderItemSchema :: TableSchema (OrderItem Name)
orderItemSchema =
  TableSchema
    { name = "order_items",
      columns =
        OrderItem
          { oiId = "id",
            oiOrderId = "order_id",
            oiProductId = "product_id",
            oiVariantId = "variant_id",
            oiProductName = "product_name",
            oiVariantLabel = "variant_label",
            oiQuantity = "quantity",
            oiUnitPriceCents = "unit_price_cents",
            oiCreatedAt = "created_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new order items.
data Insert = Insert
  { iiOrderId :: Orders.Id,
    iiProductId :: Products.Id,
    iiVariantId :: Maybe ProductVariants.Id,
    iiProductName :: Text,
    iiVariantLabel :: Text,
    iiQuantity :: Int64,
    iiUnitPriceCents :: Cents
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Insert a new order item. Returns the new item's ID.
insertOrderItem :: Insert -> Hasql.Statement () (Maybe Id)
insertOrderItem Insert {..} = interp True
  [sql|
    INSERT INTO order_items
      (order_id, product_id, variant_id,
       product_name, variant_label,
       quantity, unit_price_cents)
    VALUES
      (#{iiOrderId}, #{iiProductId}, #{iiVariantId},
       #{iiProductName}, #{iiVariantLabel},
       #{iiQuantity}, #{iiUnitPriceCents})
    RETURNING id
  |]


-- | Get all order items for a given order, ordered by ID.
getByOrderId :: Orders.Id -> Hasql.Statement () [Model]
getByOrderId orderId = interp False
  [sql|
    SELECT id, order_id, product_id, variant_id,
           product_name, variant_label,
           quantity, unit_price_cents, created_at
    FROM order_items
    WHERE order_id = #{orderId}
    ORDER BY id
  |]
