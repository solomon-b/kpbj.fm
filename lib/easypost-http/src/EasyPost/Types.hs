-- | Core EasyPost domain types.
--
-- All types in this module model EasyPost API objects and use EasyPost's
-- snake_case JSON encoding. The 'EasyPostApiKey' newtype has a censored
-- 'Show' instance so credentials never appear in logs.
module EasyPost.Types
  ( -- * API Credentials
    EasyPostApiKey (..),

    -- * Address
    Address (..),

    -- * Parcel
    Parcel (..),

    -- * Shipment
    ShipmentCreate (..),
    Shipment (..),

    -- * Rate
    Rate (..),

    -- * Purchase
    ShipmentBuy (..),

    -- * Label
    Label (..),

    -- * Errors
    EasyPostError (..),
    EasyPostFieldError (..),

    -- * Verification
    Verification (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- API Credentials

-- | EasyPost API key, used for HTTP Basic authentication.
--
-- The 'Show' instance is censored to prevent credential leakage in logs.
newtype EasyPostApiKey = EasyPostApiKey {unEasyPostApiKey :: ByteString}
  deriving newtype (Eq)

instance Show EasyPostApiKey where
  show _ = "EasyPostApiKey \"<redacted>\""

--------------------------------------------------------------------------------
-- Address

-- | A shipping address in EasyPost's format.
data Address = Address
  { -- | Full name of the recipient or sender.
    name :: Text,
    -- | Primary street line.
    street1 :: Text,
    -- | Secondary street line (apt, suite, etc.).
    street2 :: Maybe Text,
    -- | City name.
    city :: Text,
    -- | State or province code (e.g. @"CA"@).
    state :: Text,
    -- | ZIP or postal code.
    zip :: Text,
    -- | Two-letter country code (e.g. @"US"@).
    country :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Address where
  toJSON addr =
    Aeson.object $
      [ "name" Aeson..= addr.name,
        "street1" Aeson..= addr.street1,
        "city" Aeson..= addr.city,
        "state" Aeson..= addr.state,
        "zip" Aeson..= addr.zip,
        "country" Aeson..= addr.country
      ]
        <> maybe [] (\s2 -> ["street2" Aeson..= s2]) addr.street2

instance FromJSON Address where
  parseJSON = Aeson.withObject "Address" $ \o ->
    Address
      <$> o Aeson..: "name"
      <*> o Aeson..: "street1"
      <*> o Aeson..:? "street2"
      <*> o Aeson..: "city"
      <*> o Aeson..: "state"
      <*> o Aeson..: "zip"
      <*> o Aeson..: "country"

--------------------------------------------------------------------------------
-- Parcel

-- | A parcel description for shipment rating.
newtype Parcel = Parcel
  { -- | Weight in ounces.
    weight :: Double
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Parcel where
  toJSON p =
    Aeson.object
      [ "weight" Aeson..= p.weight
      ]

instance FromJSON Parcel where
  parseJSON = Aeson.withObject "Parcel" $ \o ->
    Parcel
      <$> o Aeson..: "weight"

--------------------------------------------------------------------------------
-- Shipment Create

-- | Parameters for creating a new EasyPost shipment.
--
-- Serializes as @{"shipment": {"from_address": ..., "to_address": ..., "parcel": ...}}@
-- to match the EasyPost API's expected request body.
data ShipmentCreate = ShipmentCreate
  { -- | Origin address.
    fromAddress :: Address,
    -- | Destination address.
    toAddress :: Address,
    -- | Package dimensions and weight.
    parcel :: Parcel,
    -- | Address verifications to request (e.g. @["delivery"]@).
    --
    -- When empty, no @verify@ key is emitted so the request body is
    -- byte-identical to callers that don't request verification.
    verify :: [Text]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ShipmentCreate where
  toJSON sc =
    Aeson.object
      ( [ "shipment"
            Aeson..= Aeson.object
              [ "from_address" Aeson..= sc.fromAddress,
                "to_address" Aeson..= sc.toAddress,
                "parcel" Aeson..= sc.parcel
              ]
        ]
          <> verifyField
      )
    where
      -- EasyPost expects @verify@ at the ROOT of the request body, as a sibling
      -- of the @shipment@ object (same pattern as the Address API's
      -- @{"address": {...}, "verify": ...}@) — NOT nested inside @shipment@.
      -- Emitted only when non-empty so non-verifying callers are unaffected.
      verifyField
        | null sc.verify = []
        | otherwise = ["verify" Aeson..= sc.verify]

--------------------------------------------------------------------------------
-- Label

-- | A postage label returned by EasyPost after purchasing a shipment.
newtype Label = Label
  { -- | URL to download the label PDF or PNG.
    labelUrl :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Label where
  toJSON l =
    Aeson.object
      [ "label_url" Aeson..= l.labelUrl
      ]

instance FromJSON Label where
  parseJSON = Aeson.withObject "Label" $ \o ->
    Label
      <$> o Aeson..: "label_url"

--------------------------------------------------------------------------------
-- Rate

-- | A shipping rate option returned by EasyPost.
data Rate = Rate
  { -- | Unique rate identifier (e.g. @"rate_abc123"@).
    id :: Text,
    -- | Carrier name (e.g. @"USPS"@, @"UPS"@).
    carrier :: Text,
    -- | Service level (e.g. @"Priority"@, @"Express"@).
    service :: Text,
    -- | Rate amount as a string (e.g. @"7.58"@).
    rate :: Text,
    -- | Three-letter currency code (e.g. @"USD"@).
    currency :: Text,
    -- | Estimated delivery days, if available.
    deliveryDays :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Rate where
  toJSON r =
    Aeson.object $
      [ "id" Aeson..= r.id,
        "carrier" Aeson..= r.carrier,
        "service" Aeson..= r.service,
        "rate" Aeson..= r.rate,
        "currency" Aeson..= r.currency
      ]
        <> maybe [] (\d -> ["delivery_days" Aeson..= d]) r.deliveryDays

instance FromJSON Rate where
  parseJSON = Aeson.withObject "Rate" $ \o ->
    Rate
      <$> o Aeson..: "id"
      <*> o Aeson..: "carrier"
      <*> o Aeson..: "service"
      <*> o Aeson..: "rate"
      <*> o Aeson..: "currency"
      <*> o Aeson..:? "delivery_days"

--------------------------------------------------------------------------------
-- Shipment

-- | An EasyPost shipment object.
--
-- Returned from both the create and buy endpoints. After creation, 'rates'
-- will be populated. After purchasing, 'trackingCode' and 'postageLabel'
-- will be present.
data Shipment = Shipment
  { -- | Unique shipment identifier (e.g. @"shp_abc123"@).
    id :: Text,
    -- | Available shipping rates.
    rates :: [Rate],
    -- | Tracking code, present after purchase.
    trackingCode :: Maybe Text,
    -- | Postage label, present after purchase.
    postageLabel :: Maybe Label,
    -- | Delivery verification result for the to-address, when EasyPost
    -- was asked to verify the shipment and returned a verification block.
    --
    -- Parsed from @to_address.verifications.delivery@; 'Nothing' when any
    -- of those keys are absent.
    toAddressDeliveryVerification :: Maybe Verification
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Shipment where
  toJSON s =
    Aeson.object $
      [ "id" Aeson..= s.id,
        "rates" Aeson..= s.rates
      ]
        <> maybe [] (\tc -> ["tracking_code" Aeson..= tc]) s.trackingCode
        <> maybe [] (\pl -> ["postage_label" Aeson..= pl]) s.postageLabel

instance FromJSON Shipment where
  parseJSON = Aeson.withObject "Shipment" $ \o -> do
    shipmentId <- o Aeson..: "id"
    rates <- o Aeson..:? "rates" Aeson..!= []
    trackingCode <- o Aeson..:? "tracking_code"
    postageLabel <- o Aeson..:? "postage_label"
    -- Dig through @to_address.verifications.delivery@ defensively: any
    -- missing intermediate key yields 'Nothing' rather than a parse error.
    mToAddress <- o Aeson..:? "to_address" :: Aeson.Parser (Maybe Aeson.Object)
    deliveryVerification <- case mToAddress of
      Nothing -> pure Nothing
      Just toAddr -> do
        mVerifications <- toAddr Aeson..:? "verifications" :: Aeson.Parser (Maybe Aeson.Object)
        case mVerifications of
          Nothing -> pure Nothing
          Just verifications -> verifications Aeson..:? "delivery"
    pure
      Shipment
        { id = shipmentId,
          rates = rates,
          trackingCode = trackingCode,
          postageLabel = postageLabel,
          toAddressDeliveryVerification = deliveryVerification
        }

--------------------------------------------------------------------------------
-- Errors

-- | A single field-level error reported by EasyPost.
--
-- EasyPost returns @errors@ array items as either an object with
-- @field@/@message@/@suggestion@ keys, or a bare JSON string. Both shapes
-- decode here; a bare string populates 'message' with the other fields empty.
data EasyPostFieldError = EasyPostFieldError
  { -- | The offending request field, when identified.
    field :: Maybe Text,
    -- | Human-readable description of the problem.
    message :: Text,
    -- | Suggested correction, when EasyPost offers one.
    suggestion :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EasyPostFieldError where
  parseJSON = \case
    Aeson.String s -> pure (EasyPostFieldError Nothing s Nothing)
    v ->
      flip (Aeson.withObject "EasyPostFieldError") v $ \o ->
        EasyPostFieldError
          <$> o Aeson..:? "field"
          <*> o Aeson..: "message"
          <*> o Aeson..:? "suggestion"

-- | A structured EasyPost API error, unwrapped from the @{"error": {...}}@ envelope.
data EasyPostError = EasyPostError
  { -- | Machine-readable error code (e.g. @"ADDRESS.VERIFY.FAILURE"@).
    code :: Text,
    -- | Top-level human-readable message.
    message :: Text,
    -- | Field-level errors, when present.
    errors :: [EasyPostFieldError]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EasyPostError where
  parseJSON = Aeson.withObject "EasyPostError" $ \envelope -> do
    o <- envelope Aeson..: "error"
    EasyPostError
      <$> o Aeson..: "code"
      <*> o Aeson..: "message"
      <*> o Aeson..:? "errors" Aeson..!= []

--------------------------------------------------------------------------------
-- Verification

-- | An EasyPost address verification result.
--
-- Parsed from a shipment's @to_address.verifications.delivery@ block.
data Verification = Verification
  { -- | Whether the address passed verification.
    success :: Bool,
    -- | Verification errors, when the address failed.
    errors :: [EasyPostFieldError]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Verification where
  parseJSON = Aeson.withObject "Verification" $ \o ->
    Verification
      <$> o Aeson..: "success"
      <*> o Aeson..:? "errors" Aeson..!= []

--------------------------------------------------------------------------------
-- Shipment Buy

-- | Parameters for purchasing a shipment at a specific rate.
--
-- Serializes as @{"rate": {"id": ...}}@ to match the EasyPost API's
-- expected request body.
newtype ShipmentBuy = ShipmentBuy
  { -- | The rate identifier to purchase.
    rateId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ShipmentBuy where
  toJSON sb =
    Aeson.object
      [ "rate"
          Aeson..= Aeson.object
            [ "id" Aeson..= sb.rateId
            ]
      ]
