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
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
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
    parcel :: Parcel
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ShipmentCreate where
  toJSON sc =
    Aeson.object
      [ "shipment"
          Aeson..= Aeson.object
            [ "from_address" Aeson..= sc.fromAddress,
              "to_address" Aeson..= sc.toAddress,
              "parcel" Aeson..= sc.parcel
            ]
      ]

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
    postageLabel :: Maybe Label
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
  parseJSON = Aeson.withObject "Shipment" $ \o ->
    Shipment
      <$> o Aeson..: "id"
      <*> o Aeson..:? "rates" Aeson..!= []
      <*> o Aeson..:? "tracking_code"
      <*> o Aeson..:? "postage_label"

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
