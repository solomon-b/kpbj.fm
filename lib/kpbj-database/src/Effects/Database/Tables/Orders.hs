{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Database table definition and queries for @orders@.
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

    -- * Insert Type
    Insert (..),

    -- * Queries
    insertOrder,
    getById,
    getByOrderNumber,
    getByStripeCheckoutSessionId,
    getByStripePaymentIntentId,
    updateStatus,
    updateStripeCheckoutSessionId,
    updateStripePaymentIntentId,
    updateTracking,
    updateNotes,
    listOrders,
    cancelStalePendingOrders,
    nextOrderNumber,
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
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Enum, Insert, Order)
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

instance Servant.FromHttpApiData OrderStatus where
  parseQueryParam = \case
    "pending" -> Right Pending
    "paid" -> Right Paid
    "shipped" -> Right Shipped
    "completed" -> Right Completed
    "cancelled" -> Right Cancelled
    "refunded" -> Right Refunded
    other -> Left $ "Unknown order status: " <> other

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
    oStripeCheckoutSessionId :: Column f (Maybe Text),
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
            oStripeCheckoutSessionId = "stripe_checkout_session_id",
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

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new orders.
data Insert = Insert
  { oiOrderNumber :: Text,
    oiEmail :: Text,
    oiShippingFirstName :: Text,
    oiShippingLastName :: Text,
    oiShippingAddressLine1 :: Text,
    oiShippingAddressLine2 :: Text,
    oiShippingCity :: Text,
    oiShippingState :: Text,
    oiShippingZip :: Text,
    oiShippingCountry :: Text,
    oiShippingMethod :: Text,
    oiSubtotalCents :: Cents,
    oiShippingCents :: Cents,
    oiTaxCents :: Cents,
    oiTotalCents :: Cents,
    oiPaymentMethod :: PaymentMethod,
    oiStripeCheckoutSessionId :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Insert a new order. Status defaults to 'pending'.
insertOrder :: Insert -> Hasql.Statement () (Maybe Id)
insertOrder Insert {..} = interp True
  [sql|
    INSERT INTO orders
      (order_number, email,
       shipping_first_name, shipping_last_name,
       shipping_address_line1, shipping_address_line2,
       shipping_city, shipping_state, shipping_zip, shipping_country,
       shipping_method,
       subtotal_cents, shipping_cents, tax_cents, total_cents,
       payment_method, stripe_checkout_session_id)
    VALUES
      (#{oiOrderNumber}, #{oiEmail},
       #{oiShippingFirstName}, #{oiShippingLastName},
       #{oiShippingAddressLine1}, #{oiShippingAddressLine2},
       #{oiShippingCity}, #{oiShippingState}, #{oiShippingZip}, #{oiShippingCountry},
       #{oiShippingMethod},
       #{oiSubtotalCents}, #{oiShippingCents}, #{oiTaxCents}, #{oiTotalCents},
       #{oiPaymentMethod}, #{oiStripeCheckoutSessionId})
    RETURNING id
  |]


-- | Get an order by ID.
getById :: Id -> Hasql.Statement () (Maybe Model)
getById orderId = interp False
  [sql|
    SELECT id, order_number, email, status,
           shipping_first_name, shipping_last_name,
           shipping_address_line1, shipping_address_line2,
           shipping_city, shipping_state, shipping_zip, shipping_country,
           shipping_method,
           subtotal_cents, shipping_cents, tax_cents, total_cents,
           stripe_payment_intent_id, stripe_checkout_session_id,
           paypal_order_id, payment_method,
           easypost_shipment_id, tracking_number, label_url,
           notes, created_at, updated_at
    FROM orders
    WHERE id = #{orderId}
  |]


-- | Get an order by order number.
getByOrderNumber :: Text -> Hasql.Statement () (Maybe Model)
getByOrderNumber orderNumber = interp False
  [sql|
    SELECT id, order_number, email, status,
           shipping_first_name, shipping_last_name,
           shipping_address_line1, shipping_address_line2,
           shipping_city, shipping_state, shipping_zip, shipping_country,
           shipping_method,
           subtotal_cents, shipping_cents, tax_cents, total_cents,
           stripe_payment_intent_id, stripe_checkout_session_id,
           paypal_order_id, payment_method,
           easypost_shipment_id, tracking_number, label_url,
           notes, created_at, updated_at
    FROM orders
    WHERE order_number = #{orderNumber}
  |]


-- | Get an order by Stripe Checkout Session ID.
getByStripeCheckoutSessionId :: Text -> Hasql.Statement () (Maybe Model)
getByStripeCheckoutSessionId sessionId = interp False
  [sql|
    SELECT id, order_number, email, status,
           shipping_first_name, shipping_last_name,
           shipping_address_line1, shipping_address_line2,
           shipping_city, shipping_state, shipping_zip, shipping_country,
           shipping_method,
           subtotal_cents, shipping_cents, tax_cents, total_cents,
           stripe_payment_intent_id, stripe_checkout_session_id,
           paypal_order_id, payment_method,
           easypost_shipment_id, tracking_number, label_url,
           notes, created_at, updated_at
    FROM orders
    WHERE stripe_checkout_session_id = #{sessionId}
  |]


-- | Get an order by Stripe Payment Intent ID.
getByStripePaymentIntentId :: Text -> Hasql.Statement () (Maybe Model)
getByStripePaymentIntentId paymentIntentId = interp False
  [sql|
    SELECT id, order_number, email, status,
           shipping_first_name, shipping_last_name,
           shipping_address_line1, shipping_address_line2,
           shipping_city, shipping_state, shipping_zip, shipping_country,
           shipping_method,
           subtotal_cents, shipping_cents, tax_cents, total_cents,
           stripe_payment_intent_id, stripe_checkout_session_id,
           paypal_order_id, payment_method,
           easypost_shipment_id, tracking_number, label_url,
           notes, created_at, updated_at
    FROM orders
    WHERE stripe_payment_intent_id = #{paymentIntentId}
  |]


-- | Update the status of an order. Returns the updated order.
updateStatus :: Id -> OrderStatus -> Hasql.Statement () (Maybe Model)
updateStatus orderId status = interp False
  [sql|
    UPDATE orders
    SET status = #{status}, updated_at = NOW()
    WHERE id = #{orderId}
    RETURNING id, order_number, email, status,
              shipping_first_name, shipping_last_name,
              shipping_address_line1, shipping_address_line2,
              shipping_city, shipping_state, shipping_zip, shipping_country,
              shipping_method,
              subtotal_cents, shipping_cents, tax_cents, total_cents,
              stripe_payment_intent_id, stripe_checkout_session_id,
              paypal_order_id, payment_method,
              easypost_shipment_id, tracking_number, label_url,
              notes, created_at, updated_at
  |]


-- | Update the Stripe Checkout Session ID for an order.
updateStripeCheckoutSessionId :: Id -> Text -> Hasql.Statement () ()
updateStripeCheckoutSessionId orderId sessionId = interp True
  [sql|
    UPDATE orders
    SET stripe_checkout_session_id = #{sessionId}, updated_at = NOW()
    WHERE id = #{orderId}
  |]


-- | Update the Stripe Payment Intent ID for an order.
updateStripePaymentIntentId :: Id -> Text -> Hasql.Statement () ()
updateStripePaymentIntentId orderId paymentIntentId = interp True
  [sql|
    UPDATE orders
    SET stripe_payment_intent_id = #{paymentIntentId}, updated_at = NOW()
    WHERE id = #{orderId}
  |]


-- | Update tracking information for an order.
updateTracking ::
  Id ->           -- ^ Order ID
  Text ->         -- ^ Tracking number
  Maybe Text ->   -- ^ EasyPost shipment ID
  Maybe Text ->   -- ^ Label URL
  Hasql.Statement () ()
updateTracking orderId trackingNumber easypostShipmentId labelUrl = interp True
  [sql|
    UPDATE orders
    SET tracking_number = #{trackingNumber},
        easypost_shipment_id = #{easypostShipmentId},
        label_url = #{labelUrl},
        updated_at = NOW()
    WHERE id = #{orderId}
  |]


-- | Update the notes for an order.
updateNotes :: Id -> Text -> Hasql.Statement () ()
updateNotes orderId notes = interp True
  [sql|
    UPDATE orders
    SET notes = #{notes}, updated_at = NOW()
    WHERE id = #{orderId}
  |]


-- | List orders, optionally filtered by status. Ordered by created_at DESC.
listOrders :: Maybe OrderStatus -> Hasql.Statement () [Model]
listOrders = \case
  Nothing -> listAllOrders
  Just status -> listOrdersByStatus status


-- | List all orders ordered by created_at DESC.
listAllOrders :: Hasql.Statement () [Model]
listAllOrders = interp False
  [sql|
    SELECT id, order_number, email, status,
           shipping_first_name, shipping_last_name,
           shipping_address_line1, shipping_address_line2,
           shipping_city, shipping_state, shipping_zip, shipping_country,
           shipping_method,
           subtotal_cents, shipping_cents, tax_cents, total_cents,
           stripe_payment_intent_id, stripe_checkout_session_id,
           paypal_order_id, payment_method,
           easypost_shipment_id, tracking_number, label_url,
           notes, created_at, updated_at
    FROM orders
    ORDER BY created_at DESC
  |]


-- | List orders filtered by status, ordered by created_at DESC.
listOrdersByStatus :: OrderStatus -> Hasql.Statement () [Model]
listOrdersByStatus status = interp False
  [sql|
    SELECT id, order_number, email, status,
           shipping_first_name, shipping_last_name,
           shipping_address_line1, shipping_address_line2,
           shipping_city, shipping_state, shipping_zip, shipping_country,
           shipping_method,
           subtotal_cents, shipping_cents, tax_cents, total_cents,
           stripe_payment_intent_id, stripe_checkout_session_id,
           paypal_order_id, payment_method,
           easypost_shipment_id, tracking_number, label_url,
           notes, created_at, updated_at
    FROM orders
    WHERE status = #{status}
    ORDER BY created_at DESC
  |]


-- | Cancel pending orders older than 30 minutes. Returns the IDs of
-- cancelled orders so the caller can restore their inventory.
cancelStalePendingOrders :: Hasql.Statement () [Id]
cancelStalePendingOrders = interp True
  [sql|
    UPDATE orders
    SET status = 'cancelled', updated_at = NOW()
    WHERE status = 'pending' AND created_at < NOW() - INTERVAL '30 minutes'
    RETURNING id
  |]


-- | Get the next order number from the sequence, formatted as KPBJ-NNNN.
-- Returns Maybe Text because interp infers Maybe for single-row SELECTs.
-- nextval always succeeds so Nothing should never occur in practice.
nextOrderNumber :: Hasql.Statement () (Maybe Text)
nextOrderNumber =
  fmap (fmap getOneColumn) $
    interp False
      [sql| SELECT 'KPBJ-' || LPAD(nextval('order_number_seq')::TEXT, 4, '0') |]
