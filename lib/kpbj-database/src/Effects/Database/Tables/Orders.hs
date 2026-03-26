{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Database table definition for @orders@.
--
-- Schema-only module — queries are implemented in Phase 3.
module Effects.Database.Tables.Orders
  ( -- * Id Type
    Id (..),

    -- * Enums
    OrderStatus (..),
    PaymentMethod (..),

    -- * Table Definition
    Order (..),
    orderSchema,

    -- * Model (Result alias)
    Model,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Domain.Types.Cents (Cents)
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..))
import OrphanInstances.Rel8 ()
import Rel8 hiding (Enum, Order)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for order primary keys.
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
-- Enums

-- | Order lifecycle status.
data OrderStatus = Pending | Paid | Shipped | Completed | Cancelled | Refunded
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance DBType OrderStatus where
  typeInformation =
    parseTypeInformation
      ( \case
          "pending" -> Right Pending
          "paid" -> Right Paid
          "shipped" -> Right Shipped
          "completed" -> Right Completed
          "cancelled" -> Right Cancelled
          "refunded" -> Right Refunded
          other -> Left $ "Invalid OrderStatus: " <> Text.unpack other
      )
      ( \case
          Pending -> "pending"
          Paid -> "paid"
          Shipped -> "shipped"
          Completed -> "completed"
          Cancelled -> "cancelled"
          Refunded -> "refunded"
      )
      typeInformation

instance DBEq OrderStatus

instance DecodeValue OrderStatus where
  decodeValue = Decoders.enum $ \case
    "pending" -> Just Pending
    "paid" -> Just Paid
    "shipped" -> Just Shipped
    "completed" -> Just Completed
    "cancelled" -> Just Cancelled
    "refunded" -> Just Refunded
    _ -> Nothing

instance EncodeValue OrderStatus where
  encodeValue = Encoders.enum $ \case
    Pending -> "pending"
    Paid -> "paid"
    Shipped -> "shipped"
    Completed -> "completed"
    Cancelled -> "cancelled"
    Refunded -> "refunded"

instance Display OrderStatus where
  displayBuilder = \case
    Pending -> "Pending"
    Paid -> "Paid"
    Shipped -> "Shipped"
    Completed -> "Completed"
    Cancelled -> "Cancelled"
    Refunded -> "Refunded"

-- | Payment provider used for the order.
data PaymentMethod = Stripe | Paypal
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance DBType PaymentMethod where
  typeInformation =
    parseTypeInformation
      ( \case
          "stripe" -> Right Stripe
          "paypal" -> Right Paypal
          other -> Left $ "Invalid PaymentMethod: " <> Text.unpack other
      )
      ( \case
          Stripe -> "stripe"
          Paypal -> "paypal"
      )
      typeInformation

instance DBEq PaymentMethod

instance DecodeValue PaymentMethod where
  decodeValue = Decoders.enum $ \case
    "stripe" -> Just Stripe
    "paypal" -> Just Paypal
    _ -> Nothing

instance EncodeValue PaymentMethod where
  encodeValue = Encoders.enum $ \case
    Stripe -> "stripe"
    Paypal -> "paypal"

instance Display PaymentMethod where
  displayBuilder = \case
    Stripe -> "Stripe"
    Paypal -> "PayPal"

--------------------------------------------------------------------------------
-- Table Definition

-- | The @orders@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data Order f = Order
  { oId :: Column f Id,
    oOrderNumber :: Column f Text,
    oEmail :: Column f Text,
    oStatus :: Column f OrderStatus,
    oShippingFirstName :: Column f Text,
    oShippingLastName :: Column f Text,
    oShippingAddressLine1 :: Column f Text,
    oShippingAddressLine2 :: Column f Text,
    oShippingCity :: Column f Text,
    oShippingState :: Column f Text,
    oShippingZip :: Column f Text,
    oShippingCountry :: Column f Text,
    oShippingMethod :: Column f Text,
    oSubtotalCents :: Column f Cents,
    oShippingCents :: Column f Cents,
    oTaxCents :: Column f Cents,
    oTotalCents :: Column f Cents,
    oStripePaymentIntentId :: Column f (Maybe Text),
    oPaypalOrderId :: Column f (Maybe Text),
    oPaymentMethod :: Column f PaymentMethod,
    oEasypostShipmentId :: Column f (Maybe Text),
    oTrackingNumber :: Column f (Maybe Text),
    oLabelUrl :: Column f (Maybe Text),
    oNotes :: Column f Text,
    oCreatedAt :: Column f UTCTime,
    oUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (Order f)

deriving stock instance (f ~ Result) => Eq (Order f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (Order Result)

-- | Display instance for Order Result.
instance Display (Order Result) where
  displayBuilder o =
    "Order { id = "
      <> displayBuilder o.oId
      <> ", orderNumber = "
      <> displayBuilder o.oOrderNumber
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @Order Result@.
type Model = Order Result

-- | Table schema connecting the Haskell type to the database table.
orderSchema :: TableSchema (Order Name)
orderSchema =
  TableSchema
    { name = "orders",
      columns =
        Order
          { oId = "id",
            oOrderNumber = "order_number",
            oEmail = "email",
            oStatus = "status",
            oShippingFirstName = "shipping_first_name",
            oShippingLastName = "shipping_last_name",
            oShippingAddressLine1 = "shipping_address_line1",
            oShippingAddressLine2 = "shipping_address_line2",
            oShippingCity = "shipping_city",
            oShippingState = "shipping_state",
            oShippingZip = "shipping_zip",
            oShippingCountry = "shipping_country",
            oShippingMethod = "shipping_method",
            oSubtotalCents = "subtotal_cents",
            oShippingCents = "shipping_cents",
            oTaxCents = "tax_cents",
            oTotalCents = "total_cents",
            oStripePaymentIntentId = "stripe_payment_intent_id",
            oPaypalOrderId = "paypal_order_id",
            oPaymentMethod = "payment_method",
            oEasypostShipmentId = "easypost_shipment_id",
            oTrackingNumber = "tracking_number",
            oLabelUrl = "label_url",
            oNotes = "notes",
            oCreatedAt = "created_at",
            oUpdatedAt = "updated_at"
          }
    }
