{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Database table definition for @order_items@.
--
-- Schema-only module — queries are implemented in Phase 3.
module Effects.Database.Tables.OrderItems
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    OrderItem (..),
    orderItemSchema,

    -- * Model (Result alias)
    Model,
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
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..))
import OrphanInstances.Rel8 ()
import Rel8
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
