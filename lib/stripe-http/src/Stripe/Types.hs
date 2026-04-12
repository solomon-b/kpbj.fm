-- | Core Stripe domain types.
--
-- All types in this module model Stripe API objects and use Stripe's
-- snake_case JSON encoding. Key and secret newtypes have censored 'Show'
-- instances so credentials never appear in logs.
module Stripe.Types
  ( -- * API Credentials
    StripeSecretKey (..),
    StripePublishableKey (..),
    StripeWebhookSecret (..),

    -- * Identifiers
    StripeSessionId (..),

    -- * Currency
    Currency (..),

    -- * Payment Intents
    PaymentIntentStatus (..),
    PaymentIntentCreate (..),
    PaymentIntent (..),

    -- * Checkout Sessions
    CheckoutSessionCreate (..),
    LineItem (..),
    PriceData (..),
    ProductData (..),
    ShippingOption (..),
    ShippingRateData (..),
    FixedAmount (..),
    CheckoutSession (..),
    CheckoutSessionStatus (..),
    PaymentStatus (..),

    -- * Webhook Events
    WebhookEventData (..),
    WebhookEvent (..),
    eventPaymentIntent,
    eventCheckoutSession,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Web.FormUrlEncoded (ToForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------
-- API Credentials

-- | Stripe secret key, used for server-side API authentication.
--
-- The 'Show' instance is censored to prevent credential leakage in logs.
newtype StripeSecretKey = StripeSecretKey {unStripeSecretKey :: ByteString}
  deriving newtype (Eq)

instance Show StripeSecretKey where
  show _ = "StripeSecretKey \"<redacted>\""

-- | Stripe publishable key, used in client-side JavaScript.
--
-- The 'Show' instance is censored to prevent credential leakage in logs.
newtype StripePublishableKey = StripePublishableKey {unStripePublishableKey :: Text}
  deriving newtype (Eq)

instance Show StripePublishableKey where
  show _ = "StripePublishableKey \"<redacted>\""

-- | Stripe webhook signing secret, used to verify webhook signatures.
--
-- The 'Show' instance is censored to prevent credential leakage in logs.
newtype StripeWebhookSecret = StripeWebhookSecret {unStripeWebhookSecret :: ByteString}
  deriving newtype (Eq)

instance Show StripeWebhookSecret where
  show _ = "StripeWebhookSecret \"<redacted>\""

--------------------------------------------------------------------------------
-- Identifiers

-- | A Stripe Checkout Session identifier (e.g. @cs_test_abc123@).
newtype StripeSessionId = StripeSessionId {unStripeSessionId :: Text}
  deriving newtype (Eq, Show, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Currency

-- | Supported currencies. Only USD for now.
data Currency = USD
  deriving stock (Show, Eq, Generic)

instance ToJSON Currency where
  toJSON USD = Aeson.String "usd"

instance FromJSON Currency where
  parseJSON = Aeson.withText "Currency" $ \case
    "usd" -> pure USD
    other -> fail $ "Unknown currency: " <> Text.unpack other

--------------------------------------------------------------------------------
-- Payment Intent Status

-- | All possible statuses for a Stripe PaymentIntent.
--
-- See https://docs.stripe.com/docs/payments/intents#intent-statuses
data PaymentIntentStatus
  = RequiresPaymentMethod
  | RequiresConfirmation
  | RequiresAction
  | Processing
  | RequiresCapture
  | Canceled
  | Succeeded
  deriving stock (Show, Eq, Generic)

instance ToJSON PaymentIntentStatus where
  toJSON = \case
    RequiresPaymentMethod -> Aeson.String "requires_payment_method"
    RequiresConfirmation -> Aeson.String "requires_confirmation"
    RequiresAction -> Aeson.String "requires_action"
    Processing -> Aeson.String "processing"
    RequiresCapture -> Aeson.String "requires_capture"
    Canceled -> Aeson.String "canceled"
    Succeeded -> Aeson.String "succeeded"

instance FromJSON PaymentIntentStatus where
  parseJSON = Aeson.withText "PaymentIntentStatus" $ \case
    "requires_payment_method" -> pure RequiresPaymentMethod
    "requires_confirmation" -> pure RequiresConfirmation
    "requires_action" -> pure RequiresAction
    "processing" -> pure Processing
    "requires_capture" -> pure RequiresCapture
    "canceled" -> pure Canceled
    "succeeded" -> pure Succeeded
    other -> fail $ "Unknown payment intent status: " <> Text.unpack other

--------------------------------------------------------------------------------
-- Payment Intent Create

-- | Parameters for creating a Stripe PaymentIntent.
--
-- The 'amount' is in the smallest currency unit (e.g. cents for USD).
-- Metadata is an optional map of key-value pairs attached to the object.
data PaymentIntentCreate = PaymentIntentCreate
  { -- | Amount in smallest currency unit (e.g. cents).
    amount :: Int,
    -- | Three-letter currency code.
    currency :: Currency,
    -- | Arbitrary key-value metadata.
    metadata :: Map Text Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PaymentIntentCreate where
  toJSON pic =
    Aeson.object $
      [ "amount" Aeson..= pic.amount,
        "currency" Aeson..= pic.currency
      ]
        <> if Map.null pic.metadata
          then []
          else ["metadata" Aeson..= pic.metadata]

-- | Form encoding for Stripe's @application/x-www-form-urlencoded@ POST body.
--
-- Metadata is encoded as @metadata[key]=value@ pairs. If the 'ToForm' generic
-- machinery does not handle this nesting cleanly, the instance builds the form
-- manually.
instance ToForm PaymentIntentCreate where
  toForm pic =
    let currencyText :: Text
        currencyText = case pic.currency of
          USD -> "usd"

        basePairs :: [(Text, Text)]
        basePairs =
          [ ("amount", Text.pack (show pic.amount)),
            ("currency", currencyText)
          ]

        metadataPairs :: [(Text, Text)]
        metadataPairs =
          [ ("metadata[" <> k <> "]", v)
            | (k, v) <- Map.toList pic.metadata
          ]
     in Form.toForm (basePairs <> metadataPairs)

--------------------------------------------------------------------------------
-- Payment Intent

-- | A Stripe PaymentIntent object returned from the API.
--
-- Only the fields needed by this application are decoded; Stripe returns
-- many more fields which are silently ignored by the 'FromJSON' instance.
data PaymentIntent = PaymentIntent
  { -- | Unique identifier (e.g. @pi_1234@).
    id :: Text,
    -- | Client secret for client-side confirmation. Present when the
    -- PaymentIntent is in a confirmable state.
    clientSecret :: Maybe Text,
    -- | Current status of the PaymentIntent.
    status :: PaymentIntentStatus,
    -- | Amount in smallest currency unit.
    amount :: Int,
    -- | Three-letter currency code.
    currency :: Currency
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PaymentIntent where
  toJSON pi' =
    Aeson.object
      [ "id" Aeson..= pi'.id,
        "client_secret" Aeson..= pi'.clientSecret,
        "status" Aeson..= pi'.status,
        "amount" Aeson..= pi'.amount,
        "currency" Aeson..= pi'.currency
      ]

instance FromJSON PaymentIntent where
  parseJSON = Aeson.withObject "PaymentIntent" $ \o ->
    PaymentIntent
      <$> o Aeson..: "id"
      <*> o Aeson..:? "client_secret"
      <*> o Aeson..: "status"
      <*> o Aeson..: "amount"
      <*> o Aeson..: "currency"

--------------------------------------------------------------------------------
-- Checkout Session Status

-- | Status of a Checkout Session.
data CheckoutSessionStatus
  = Open
  | Complete
  | Expired
  deriving stock (Show, Eq, Generic)

instance ToJSON CheckoutSessionStatus where
  toJSON = \case
    Open -> Aeson.String "open"
    Complete -> Aeson.String "complete"
    Expired -> Aeson.String "expired"

instance FromJSON CheckoutSessionStatus where
  parseJSON = Aeson.withText "CheckoutSessionStatus" $ \case
    "open" -> pure Open
    "complete" -> pure Complete
    "expired" -> pure Expired
    other -> fail $ "Unknown checkout session status: " <> Text.unpack other

--------------------------------------------------------------------------------
-- Payment Status

-- | Payment status of a Checkout Session.
data PaymentStatus
  = Unpaid
  | Paid
  | NoPaymentRequired
  deriving stock (Show, Eq, Generic)

instance ToJSON PaymentStatus where
  toJSON = \case
    Unpaid -> Aeson.String "unpaid"
    Paid -> Aeson.String "paid"
    NoPaymentRequired -> Aeson.String "no_payment_required"

instance FromJSON PaymentStatus where
  parseJSON = Aeson.withText "PaymentStatus" $ \case
    "unpaid" -> pure Unpaid
    "paid" -> pure Paid
    "no_payment_required" -> pure NoPaymentRequired
    other -> fail $ "Unknown payment status: " <> Text.unpack other

--------------------------------------------------------------------------------
-- Checkout Session Create (nested types)

-- | Product information embedded in a line item's price data.
data ProductData = ProductData
  { -- | Product name displayed to the customer.
    name :: Text,
    -- | Optional product description.
    description :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

-- | Inline price definition for a line item.
data PriceData = PriceData
  { -- | Three-letter currency code.
    currency :: Currency,
    -- | Amount in smallest currency unit (e.g. cents).
    unitAmount :: Int,
    -- | Product details.
    productData :: ProductData
  }
  deriving stock (Show, Eq, Generic)

-- | A line item in a Checkout Session.
data LineItem = LineItem
  { -- | Inline price definition.
    priceData :: PriceData,
    -- | Number of units.
    quantity :: Int
  }
  deriving stock (Show, Eq, Generic)

-- | Amount and currency for a fixed shipping rate.
data FixedAmount = FixedAmount
  { -- | Shipping cost in smallest currency unit (e.g. cents).
    amount :: Int,
    -- | Three-letter currency code.
    currency :: Currency
  }
  deriving stock (Show, Eq, Generic)

-- | Details for a shipping rate.
data ShippingRateData = ShippingRateData
  { -- | Rate type. Always @"fixed_amount"@ for pre-calculated rates.
    type_ :: Text,
    -- | Display name shown to the customer (e.g. "USPS Priority Mail (2-3 days)").
    displayName :: Text,
    -- | The fixed amount for this shipping rate.
    fixedAmount :: FixedAmount
  }
  deriving stock (Show, Eq, Generic)

-- | A shipping option presented to the customer during checkout.
data ShippingOption = ShippingOption
  { -- | Shipping rate details.
    shippingRateData :: ShippingRateData
  }
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Checkout Session Create

-- | Parameters for creating a Stripe Checkout Session.
--
-- Uses embedded (inline) pricing mode — prices and product info are
-- specified directly in the request rather than referencing pre-created
-- Stripe Price objects.
data CheckoutSessionCreate = CheckoutSessionCreate
  { -- | Session mode. Use @"payment"@ for one-time payments.
    mode :: Text,
    -- | UI mode. Use @"embedded"@ for Stripe Elements integration.
    uiMode :: Text,
    -- | URL to redirect the customer after payment. Must contain
    -- @{CHECKOUT_SESSION_ID}@ as a placeholder.
    returnUrl :: Text,
    -- | Pre-fill the customer's email address.
    customerEmail :: Maybe Text,
    -- | Products being purchased.
    lineItems :: [LineItem],
    -- | Available shipping rate options.
    shippingOptions :: [ShippingOption],
    -- | Allowed payment method types (e.g. @["card"]@).
    paymentMethodTypes :: [Text],
    -- | Arbitrary key-value metadata (e.g. order number).
    metadata :: Map Text Text
  }
  deriving stock (Show, Eq, Generic)

-- | Form encoding for Stripe's @application/x-www-form-urlencoded@ POST body.
--
-- Stripe requires bracket-indexed notation for nested objects:
--
-- @
-- line_items[0][price_data][currency]=usd
-- line_items[0][price_data][unit_amount]=2000
-- line_items[0][price_data][product_data][name]=T-shirt
-- line_items[0][quantity]=2
-- @
instance ToForm CheckoutSessionCreate where
  toForm csc =
    let basePairs :: [(Text, Text)]
        basePairs =
          [ ("mode", csc.mode),
            ("ui_mode", csc.uiMode),
            ("return_url", csc.returnUrl)
          ]

        emailPairs :: [(Text, Text)]
        emailPairs = case csc.customerEmail of
          Nothing -> []
          Just email -> [("customer_email", email)]

        lineItemPairs :: [(Text, Text)]
        lineItemPairs = concatMap (uncurry encodeLineItem) (zip [0 :: Int ..] csc.lineItems)

        shippingPairs :: [(Text, Text)]
        shippingPairs = concatMap (uncurry encodeShippingOption) (zip [0 :: Int ..] csc.shippingOptions)

        pmtPairs :: [(Text, Text)]
        pmtPairs =
          [ ("payment_method_types[" <> Text.pack (show idx) <> "]", t)
            | (idx, t) <- zip [0 :: Int ..] csc.paymentMethodTypes
          ]

        metadataPairs :: [(Text, Text)]
        metadataPairs =
          [ ("metadata[" <> k <> "]", v)
            | (k, v) <- Map.toList csc.metadata
          ]
     in Form.toForm $
          basePairs
            <> emailPairs
            <> lineItemPairs
            <> shippingPairs
            <> pmtPairs
            <> metadataPairs


-- | Encode a single 'LineItem' at a given index into form key-value pairs.
encodeLineItem :: Int -> LineItem -> [(Text, Text)]
encodeLineItem idx li =
  let prefix = "line_items[" <> Text.pack (show idx) <> "]"

      currencyText = case li.priceData.currency of
        USD -> "usd"

      descPairs = case li.priceData.productData.description of
        Nothing -> []
        Just desc -> [(prefix <> "[price_data][product_data][description]", desc)]
   in [ (prefix <> "[price_data][currency]", currencyText),
        (prefix <> "[price_data][unit_amount]", Text.pack (show li.priceData.unitAmount)),
        (prefix <> "[price_data][product_data][name]", li.priceData.productData.name),
        (prefix <> "[quantity]", Text.pack (show li.quantity))
      ]
        <> descPairs


-- | Encode a single 'ShippingOption' at a given index into form key-value pairs.
encodeShippingOption :: Int -> ShippingOption -> [(Text, Text)]
encodeShippingOption idx so =
  let prefix = "shipping_options[" <> Text.pack (show idx) <> "][shipping_rate_data]"

      currencyText = case so.shippingRateData.fixedAmount.currency of
        USD -> "usd"
   in [ (prefix <> "[type]", so.shippingRateData.type_),
        (prefix <> "[display_name]", so.shippingRateData.displayName),
        (prefix <> "[fixed_amount][amount]", Text.pack (show so.shippingRateData.fixedAmount.amount)),
        (prefix <> "[fixed_amount][currency]", currencyText)
      ]

--------------------------------------------------------------------------------
-- Checkout Session (response)

-- | A Stripe Checkout Session object returned from the API.
--
-- Only the fields needed by this application are decoded; Stripe returns
-- many more fields which are silently ignored by the 'FromJSON' instance.
data CheckoutSession = CheckoutSession
  { -- | Unique identifier (e.g. @cs_test_...@).
    id :: StripeSessionId,
    -- | Client secret for frontend initialization with Stripe Elements.
    clientSecret :: Maybe Text,
    -- | Current status of the session.
    status :: CheckoutSessionStatus,
    -- | Whether payment has been collected.
    paymentStatus :: PaymentStatus,
    -- | The underlying PaymentIntent identifier, if applicable.
    paymentIntent :: Maybe Text,
    -- | Total amount in smallest currency unit.
    amountTotal :: Maybe Int,
    -- | Three-letter currency code.
    currency :: Maybe Currency,
    -- | Customer email if provided.
    customerEmail :: Maybe Text,
    -- | Arbitrary key-value metadata.
    metadata :: Map Text Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CheckoutSession where
  toJSON cs =
    Aeson.object
      [ "id" Aeson..= cs.id,
        "client_secret" Aeson..= cs.clientSecret,
        "status" Aeson..= cs.status,
        "payment_status" Aeson..= cs.paymentStatus,
        "payment_intent" Aeson..= cs.paymentIntent,
        "amount_total" Aeson..= cs.amountTotal,
        "currency" Aeson..= cs.currency,
        "customer_email" Aeson..= cs.customerEmail,
        "metadata" Aeson..= cs.metadata
      ]

instance FromJSON CheckoutSession where
  parseJSON = Aeson.withObject "CheckoutSession" $ \o ->
    CheckoutSession
      <$> o Aeson..: "id"
      <*> o Aeson..:? "client_secret"
      <*> o Aeson..: "status"
      <*> o Aeson..: "payment_status"
      <*> o Aeson..:? "payment_intent"
      <*> o Aeson..:? "amount_total"
      <*> o Aeson..:? "currency"
      <*> o Aeson..:? "customer_email"
      <*> (o Aeson..:? "metadata" Aeson..!= Map.empty)

--------------------------------------------------------------------------------
-- Webhook Events

-- | The @data@ field of a Stripe webhook event.
--
-- Contains the raw JSON of the Stripe object that triggered the event.
-- Use 'eventPaymentIntent' or 'eventCheckoutSession' to extract a typed
-- object based on the event type.
newtype WebhookEventData = WebhookEventData
  { -- | The raw JSON object embedded in the event. Parse with
    -- 'eventPaymentIntent' or 'eventCheckoutSession'.
    object :: Aeson.Value
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON WebhookEventData where
  toJSON wed =
    Aeson.object
      [ "object" Aeson..= wed.object
      ]

instance FromJSON WebhookEventData where
  parseJSON = Aeson.withObject "WebhookEventData" $ \o ->
    WebhookEventData
      <$> o Aeson..: "object"

-- | A Stripe webhook event.
--
-- See https://docs.stripe.com/docs/api/events
data WebhookEvent = WebhookEvent
  { -- | Unique identifier (e.g. @evt_1234@).
    id :: Text,
    -- | Event type string (e.g. @payment_intent.succeeded@).
    type_ :: Text,
    -- | Event data containing the relevant Stripe object.
    data_ :: WebhookEventData
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON WebhookEvent where
  toJSON we =
    Aeson.object
      [ "id" Aeson..= we.id,
        "type" Aeson..= we.type_,
        "data" Aeson..= we.data_
      ]

instance FromJSON WebhookEvent where
  parseJSON = Aeson.withObject "WebhookEvent" $ \o ->
    WebhookEvent
      <$> o Aeson..: "id"
      <*> o Aeson..: "type"
      <*> o Aeson..: "data"

-- | Extract a 'PaymentIntent' from webhook event data.
--
-- Returns 'Nothing' if the embedded object is not a valid PaymentIntent.
-- Typically used with @payment_intent.*@ event types.
eventPaymentIntent :: WebhookEventData -> Maybe PaymentIntent
eventPaymentIntent wed =
  Aeson.parseMaybe Aeson.parseJSON wed.object


-- | Extract a 'CheckoutSession' from webhook event data.
--
-- Returns 'Nothing' if the embedded object is not a valid CheckoutSession.
-- Typically used with @checkout.session.*@ event types.
eventCheckoutSession :: WebhookEventData -> Maybe CheckoutSession
eventCheckoutSession wed =
  Aeson.parseMaybe Aeson.parseJSON wed.object
