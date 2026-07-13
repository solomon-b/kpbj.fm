-- | Core EasyPost domain types.
--
-- Types are split into two families:
--
-- * __Request types__ — @*Params@ records and 'ShipmentBuy'. These are what
--   you send to EasyPost; they have hand-written 'ToJSON' instances that
--   control nesting and omit optional keys when empty.
-- * __Response types__ — 'Address', 'Parcel', 'Shipment', 'Rate',
--   'PostageLabel', etc. These model the full documented shape of EasyPost's
--   API objects and only have 'FromJSON' instances.
--
-- All JSON encoding follows EasyPost's snake_case convention. The
-- 'EasyPostApiKey' newtype has a censored 'Show' instance so credentials never
-- appear in logs.
module EasyPost.Types
  ( -- * API Credentials
    EasyPostApiKey (..),

    -- * Address
    Address (..),
    AddressParams (..),

    -- * Parcel
    Parcel (..),
    ParcelParams (..),

    -- * Shipment
    Shipment (..),
    ShipmentParams (..),
    ShipmentBuy (..),

    -- * Rate
    Rate (..),

    -- * Postage Label
    PostageLabel (..),

    -- * Verification
    Verifications (..),
    Verification (..),
    VerificationDetails (..),

    -- * Errors
    EasyPostError (..),
    EasyPostFieldError (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- JSON Options

-- | Shared aeson options mapping camelCase Haskell fields to EasyPost's
-- snake_case JSON keys (e.g. @createdAt@ ↔ @created_at@).
jsonOptions :: Aeson.Options
jsonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    }

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
-- Address (response)

-- | A shipping address as returned by EasyPost.
--
-- Only @id@, @object@, @mode@, @createdAt@, and @updatedAt@ are guaranteed
-- present; all descriptive fields are optional and may be @null@.
data Address = Address
  { -- | Unique address identifier (e.g. @"adr_abc123"@).
    id :: Text,
    -- | Object type, always @"Address"@.
    object :: Text,
    -- | Environment mode, @"test"@ or @"production"@.
    mode :: Text,
    -- | Creation timestamp.
    createdAt :: UTCTime,
    -- | Last-update timestamp.
    updatedAt :: UTCTime,
    -- | Primary street line.
    street1 :: Maybe Text,
    -- | Secondary street line (apt, suite, etc.).
    street2 :: Maybe Text,
    -- | City name.
    city :: Maybe Text,
    -- | State or province code (e.g. @"CA"@).
    state :: Maybe Text,
    -- | ZIP or postal code.
    zip :: Maybe Text,
    -- | Two-letter country code (e.g. @"US"@).
    country :: Maybe Text,
    -- | Full name of the recipient or sender.
    name :: Maybe Text,
    -- | Company name.
    company :: Maybe Text,
    -- | Phone number.
    phone :: Maybe Text,
    -- | Email address.
    email :: Maybe Text,
    -- | Federal tax identifier.
    federalTaxId :: Maybe Text,
    -- | State tax identifier.
    stateTaxId :: Maybe Text,
    -- | Whether the address is residential.
    residential :: Maybe Bool,
    -- | Carrier facility hint.
    carrierFacility :: Maybe Text,
    -- | Verification results, present when verification was requested.
    verifications :: Maybe Verifications
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Address where
  parseJSON = Aeson.genericParseJSON jsonOptions

--------------------------------------------------------------------------------
-- Address (request)

-- | Parameters for creating an address.
--
-- The required address lines are always emitted; optional fields are emitted
-- only when 'Just'. The three @verify*@ lists request server-side address
-- verification and are omitted when empty.
data AddressParams = AddressParams
  { -- | Full name of the recipient or sender.
    name :: Maybe Text,
    -- | Company name.
    company :: Maybe Text,
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
    country :: Text,
    -- | Phone number.
    phone :: Maybe Text,
    -- | Email address.
    email :: Maybe Text,
    -- | Federal tax identifier.
    federalTaxId :: Maybe Text,
    -- | State tax identifier.
    stateTaxId :: Maybe Text,
    -- | Whether the address is residential.
    residential :: Maybe Bool,
    -- | Carrier facility hint.
    carrierFacility :: Maybe Text,
    -- | Verifications to request (e.g. @["delivery"]@); emitted only when
    -- non-empty.
    verify :: [Text],
    -- | Strict verifications to request; emitted only when non-empty.
    verifyStrict :: [Text],
    -- | Carrier verifications to request; emitted only when non-empty.
    verifyCarrier :: [Text]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AddressParams where
  toJSON addr =
    Aeson.object $
      [ "street1" Aeson..= addr.street1,
        "city" Aeson..= addr.city,
        "state" Aeson..= addr.state,
        "zip" Aeson..= addr.zip,
        "country" Aeson..= addr.country
      ]
        <> optional "name" addr.name
        <> optional "company" addr.company
        <> optional "street2" addr.street2
        <> optional "phone" addr.phone
        <> optional "email" addr.email
        <> optional "federal_tax_id" addr.federalTaxId
        <> optional "state_tax_id" addr.stateTaxId
        <> optional "residential" addr.residential
        <> optional "carrier_facility" addr.carrierFacility
        <> optionalList "verify" addr.verify
        <> optionalList "verify_strict" addr.verifyStrict
        <> optionalList "verify_carrier" addr.verifyCarrier

--------------------------------------------------------------------------------
-- Verification (response)

-- | The verification results attached to an address, keyed by verification
-- kind.
data Verifications = Verifications
  { -- | Deliverability verification.
    delivery :: Maybe Verification,
    -- | ZIP+4 verification.
    zip4 :: Maybe Verification
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Verifications where
  parseJSON = Aeson.genericParseJSON jsonOptions

-- | A single EasyPost address verification result.
data Verification = Verification
  { -- | Whether the address passed verification.
    success :: Bool,
    -- | Verification errors, when the address failed.
    errors :: [EasyPostFieldError],
    -- | Additional geocoding details, when available.
    details :: Maybe VerificationDetails
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Verification where
  parseJSON = Aeson.withObject "Verification" $ \o ->
    Verification
      <$> o Aeson..: "success"
      <*> o Aeson..:? "errors" Aeson..!= []
      <*> o Aeson..:? "details"

-- | Geocoding details attached to a 'Verification'.
data VerificationDetails = VerificationDetails
  { -- | Latitude of the verified address.
    latitude :: Maybe Double,
    -- | Longitude of the verified address.
    longitude :: Maybe Double,
    -- | Time zone of the verified address.
    timeZone :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON VerificationDetails where
  parseJSON = Aeson.genericParseJSON jsonOptions

--------------------------------------------------------------------------------
-- Parcel (response)

-- | A parcel as returned by EasyPost.
data Parcel = Parcel
  { -- | Unique parcel identifier (e.g. @"prcl_abc123"@).
    id :: Text,
    -- | Object type, always @"Parcel"@.
    object :: Text,
    -- | Environment mode, @"test"@ or @"production"@.
    mode :: Text,
    -- | Creation timestamp.
    createdAt :: UTCTime,
    -- | Last-update timestamp.
    updatedAt :: UTCTime,
    -- | Length in inches.
    length :: Maybe Double,
    -- | Width in inches.
    width :: Maybe Double,
    -- | Height in inches.
    height :: Maybe Double,
    -- | Weight in ounces.
    weight :: Double,
    -- | Predefined package type, when used.
    predefinedPackage :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Parcel where
  parseJSON = Aeson.genericParseJSON jsonOptions

--------------------------------------------------------------------------------
-- Parcel (request)

-- | Parameters for creating a parcel.
--
-- @weight@ is always emitted; the dimensional fields and predefined package
-- are emitted only when 'Just'.
data ParcelParams = ParcelParams
  { -- | Length in inches.
    length :: Maybe Double,
    -- | Width in inches.
    width :: Maybe Double,
    -- | Height in inches.
    height :: Maybe Double,
    -- | Weight in ounces.
    weight :: Double,
    -- | Predefined package type (e.g. @"FlatRateEnvelope"@).
    predefinedPackage :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ParcelParams where
  toJSON p =
    Aeson.object $
      [ "weight" Aeson..= p.weight
      ]
        <> optional "length" p.length
        <> optional "width" p.width
        <> optional "height" p.height
        <> optional "predefined_package" p.predefinedPackage

--------------------------------------------------------------------------------
-- Rate (response)

-- | A shipping rate option returned by EasyPost.
data Rate = Rate
  { -- | Unique rate identifier (e.g. @"rate_abc123"@).
    id :: Text,
    -- | Object type, always @"Rate"@.
    object :: Text,
    -- | Environment mode, @"test"@ or @"production"@.
    mode :: Text,
    -- | Service level (e.g. @"Priority"@, @"Express"@).
    service :: Text,
    -- | Carrier name (e.g. @"USPS"@, @"UPS"@).
    carrier :: Text,
    -- | Carrier account identifier, when a carrier account was used.
    carrierAccountId :: Maybe Text,
    -- | Identifier of the shipment this rate belongs to.
    shipmentId :: Text,
    -- | Rate amount as a string (e.g. @"7.58"@).
    rate :: Text,
    -- | Three-letter currency code for 'rate' (e.g. @"USD"@).
    currency :: Text,
    -- | Retail rate amount as a string.
    retailRate :: Maybe Text,
    -- | Currency for 'retailRate'.
    retailCurrency :: Maybe Text,
    -- | List rate amount as a string.
    listRate :: Maybe Text,
    -- | Currency for 'listRate'.
    listCurrency :: Maybe Text,
    -- | Estimated delivery days, if available.
    deliveryDays :: Maybe Int,
    -- | Estimated delivery date, if available.
    deliveryDate :: Maybe UTCTime,
    -- | Whether the delivery date is guaranteed.
    deliveryDateGuaranteed :: Bool,
    -- | Estimated delivery days (carrier estimate).
    estDeliveryDays :: Maybe Int,
    -- | Billing type for this rate.
    billingType :: Maybe Text,
    -- | Creation timestamp.
    createdAt :: UTCTime,
    -- | Last-update timestamp.
    updatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Rate where
  parseJSON = Aeson.genericParseJSON jsonOptions

--------------------------------------------------------------------------------
-- Postage Label (response)

-- | A postage label returned by EasyPost after purchasing a shipment.
--
-- Only 'labelUrl' is guaranteed present when a label exists; the format-,
-- resolution-, and alternate-URL fields are optional.
data PostageLabel = PostageLabel
  { -- | Unique label identifier (e.g. @"pl_abc123"@).
    id :: Text,
    -- | Object type, always @"PostageLabel"@.
    object :: Text,
    -- | Creation timestamp.
    createdAt :: UTCTime,
    -- | Last-update timestamp.
    updatedAt :: UTCTime,
    -- | Number of days the label date was advanced.
    dateAdvance :: Maybe Int,
    -- | Integrated form type, when present.
    integratedForm :: Maybe Text,
    -- | The label date.
    labelDate :: Maybe UTCTime,
    -- | URL to the EPL2-format label.
    labelEpl2Url :: Maybe Text,
    -- | Label file type (e.g. @"image/png"@).
    labelFileType :: Maybe Text,
    -- | URL to the PDF-format label.
    labelPdfUrl :: Maybe Text,
    -- | Label resolution in DPI.
    labelResolution :: Maybe Int,
    -- | Label size (e.g. @"4x6"@).
    labelSize :: Maybe Text,
    -- | Label type.
    labelType :: Maybe Text,
    -- | URL to download the label; always present when a label exists.
    labelUrl :: Text,
    -- | URL to the ZPL-format label.
    labelZplUrl :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PostageLabel where
  parseJSON = Aeson.genericParseJSON jsonOptions

--------------------------------------------------------------------------------
-- Shipment (response)

-- | An EasyPost shipment object.
--
-- Returned from both the create and buy endpoints. After creation, 'rates'
-- will be populated. After purchasing, 'trackingCode' and 'postageLabel'
-- will be present.
--
-- Several nested objects are not yet modeled as dedicated types and are
-- retained as raw JSON 'Value's.
data Shipment = Shipment
  { -- | Unique shipment identifier (e.g. @"shp_abc123"@).
    id :: Text,
    -- | Object type, always @"Shipment"@.
    object :: Text,
    -- | Environment mode, @"test"@ or @"production"@.
    mode :: Text,
    -- | Creation timestamp.
    createdAt :: UTCTime,
    -- | Last-update timestamp.
    updatedAt :: UTCTime,
    -- | Caller-supplied reference.
    reference :: Maybe Text,
    -- | Destination address.
    toAddress :: Address,
    -- | Origin address.
    fromAddress :: Address,
    -- | Return address, when distinct.
    returnAddress :: Maybe Address,
    -- | Buyer address, when distinct.
    buyerAddress :: Maybe Address,
    -- | Package dimensions and weight.
    parcel :: Parcel,
    -- | Customs information. Not yet modeled as a dedicated type; retained as
    -- raw JSON.
    customsInfo :: Maybe Value,
    -- | Available shipping rates.
    rates :: [Rate],
    -- | The purchased rate, present after buying.
    selectedRate :: Maybe Rate,
    -- | Postage label, present after purchase.
    postageLabel :: Maybe PostageLabel,
    -- | Tracking code, present after purchase.
    trackingCode :: Maybe Text,
    -- | Tracker object. Not yet modeled as a dedicated type; retained as raw
    -- JSON.
    tracker :: Maybe Value,
    -- | Shipment options. Not yet modeled as a dedicated type; retained as raw
    -- JSON.
    options :: Maybe Value,
    -- | Carrier messages. Not yet modeled as dedicated types; retained as raw
    -- JSON.
    messages :: [Value],
    -- | Current status.
    status :: Maybe Text,
    -- | Refund status, when a refund was requested.
    refundStatus :: Maybe Text,
    -- | Whether this is a return shipment.
    isReturn :: Bool,
    -- | Insurance amount as a string.
    insurance :: Maybe Text,
    -- | USPS zone, when applicable.
    uspsZone :: Maybe Int,
    -- | Identifier of the batch this shipment belongs to.
    batchId :: Maybe Text,
    -- | Batch status.
    batchStatus :: Maybe Text,
    -- | Batch message.
    batchMessage :: Maybe Text,
    -- | Customs/compliance forms. Not yet modeled as dedicated types; retained
    -- as raw JSON.
    forms :: [Value],
    -- | Fees applied to the shipment. Not yet modeled as dedicated types;
    -- retained as raw JSON.
    fees :: [Value],
    -- | Identifier of the order this shipment belongs to.
    orderId :: Maybe Text,
    -- | Scan form object. Not yet modeled as a dedicated type; retained as raw
    -- JSON.
    scanForm :: Maybe Value,
    -- | Line items. Not yet modeled as a dedicated type; retained as raw JSON.
    lineItems :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Shipment where
  parseJSON = Aeson.withObject "Shipment" $ \o ->
    Shipment
      <$> o Aeson..: "id"
      <*> o Aeson..: "object"
      <*> o Aeson..: "mode"
      <*> o Aeson..: "created_at"
      <*> o Aeson..: "updated_at"
      <*> o Aeson..:? "reference"
      <*> o Aeson..: "to_address"
      <*> o Aeson..: "from_address"
      <*> o Aeson..:? "return_address"
      <*> o Aeson..:? "buyer_address"
      <*> o Aeson..: "parcel"
      <*> o Aeson..:? "customs_info"
      <*> o Aeson..:? "rates" Aeson..!= []
      <*> o Aeson..:? "selected_rate"
      <*> o Aeson..:? "postage_label"
      <*> o Aeson..:? "tracking_code"
      <*> o Aeson..:? "tracker"
      <*> o Aeson..:? "options"
      <*> o Aeson..:? "messages" Aeson..!= []
      <*> o Aeson..:? "status"
      <*> o Aeson..:? "refund_status"
      <*> o Aeson..:? "is_return" Aeson..!= False
      <*> o Aeson..:? "insurance"
      <*> o Aeson..:? "usps_zone"
      <*> o Aeson..:? "batch_id"
      <*> o Aeson..:? "batch_status"
      <*> o Aeson..:? "batch_message"
      <*> o Aeson..:? "forms" Aeson..!= []
      <*> o Aeson..:? "fees" Aeson..!= []
      <*> o Aeson..:? "order_id"
      <*> o Aeson..:? "scan_form"
      <*> o Aeson..:? "line_items"

--------------------------------------------------------------------------------
-- Shipment (request)

-- | Parameters for creating a new EasyPost shipment.
--
-- Serializes with everything nested under a top-level @shipment@ object:
-- @{"shipment": {"from_address": ..., "to_address": ..., "parcel": ...}}@.
--
-- Address verification is expressed via the @verify@\/@verify_strict@\/
-- @verify_carrier@ fields on the 'AddressParams' — there is no shipment-level
-- verify key.
data ShipmentParams = ShipmentParams
  { -- | Origin address.
    fromAddress :: AddressParams,
    -- | Destination address.
    toAddress :: AddressParams,
    -- | Package dimensions and weight.
    parcel :: ParcelParams,
    -- | Carrier account identifiers to rate against; emitted only when
    -- non-empty.
    carrierAccounts :: [Text],
    -- | Requested service level.
    service :: Maybe Text,
    -- | Caller-supplied reference.
    reference :: Maybe Text,
    -- | Customs information. Not yet modeled as a dedicated type; passed
    -- through as raw JSON.
    customsInfo :: Maybe Value,
    -- | Shipment options. Not yet modeled as a dedicated type; passed through
    -- as raw JSON.
    options :: Maybe Value,
    -- | Whether this is a return shipment.
    isReturn :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ShipmentParams where
  toJSON sc =
    Aeson.object
      [ "shipment" Aeson..= Aeson.object shipmentFields
      ]
    where
      shipmentFields =
        [ "from_address" Aeson..= sc.fromAddress,
          "to_address" Aeson..= sc.toAddress,
          "parcel" Aeson..= sc.parcel
        ]
          <> optionalList "carrier_accounts" sc.carrierAccounts
          <> optional "service" sc.service
          <> optional "reference" sc.reference
          <> optional "customs_info" sc.customsInfo
          <> optional "options" sc.options
          <> optional "is_return" sc.isReturn

--------------------------------------------------------------------------------
-- Shipment Buy (request)

-- | Parameters for purchasing a shipment at a specific rate.
--
-- Serializes as @{"rate": {"id": ...}}@, plus @insurance@ when requested.
data ShipmentBuy = ShipmentBuy
  { -- | The rate identifier to purchase.
    rateId :: Text,
    -- | Insurance amount to purchase, as a string.
    insurance :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ShipmentBuy where
  toJSON sb =
    Aeson.object $
      [ "rate"
          Aeson..= Aeson.object
            [ "id" Aeson..= sb.rateId
            ]
      ]
        <> optional "insurance" sb.insurance

--------------------------------------------------------------------------------
-- Errors

-- | A single field-level error reported by EasyPost.
--
-- EasyPost returns @errors@ array items as either an object with
-- @field@\/@message@\/@suggestion@\/@code@ keys, or a bare JSON string. Both
-- shapes decode here; a bare string populates 'message' with the other fields
-- empty.
data EasyPostFieldError = EasyPostFieldError
  { -- | The offending request field, when identified.
    field :: Maybe Text,
    -- | Human-readable description of the problem.
    message :: Text,
    -- | Suggested correction, when EasyPost offers one.
    suggestion :: Maybe Text,
    -- | Machine-readable field-level code (e.g. @"E.ADDRESS.NOT_FOUND"@).
    code :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EasyPostFieldError where
  parseJSON = \case
    Aeson.String s -> pure (EasyPostFieldError Nothing s Nothing Nothing)
    v ->
      flip (Aeson.withObject "EasyPostFieldError") v $ \o ->
        EasyPostFieldError
          <$> o Aeson..:? "field"
          <*> o Aeson..: "message"
          <*> o Aeson..:? "suggestion"
          <*> o Aeson..:? "code"

-- | A structured EasyPost API error, unwrapped from the @{"error": {...}}@
-- envelope.
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
-- Encoding helpers

-- | Emit a JSON key/value pair only when the value is 'Just'.
optional :: (ToJSON a) => Aeson.Key -> Maybe a -> [Aeson.Pair]
optional key = maybe [] (\v -> [key Aeson..= v])

-- | Emit a JSON key/list pair only when the list is non-empty.
optionalList :: (ToJSON a) => Aeson.Key -> [a] -> [Aeson.Pair]
optionalList _ [] = []
optionalList key xs = [key Aeson..= xs]
