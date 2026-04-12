module API.Store.Types
  ( CartItem (..),
    CartRequest,
    ShippingRateRequest (..),
    CreateSessionRequest (..),
    CreateSessionResponse (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson qualified as Aeson
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data CartItem = CartItem
  { productId :: Products.Id,
    variantId :: Maybe ProductVariants.Id,
    quantity :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

type CartRequest = [CartItem]

--------------------------------------------------------------------------------

-- | Checkout request to the shipping rates endpoint.
--
-- JSON fields: @email@, @first_name@, @last_name@, @address_line1@, @address_line2@,
-- @city@, @state@, @zip@, @cart@
data ShippingRateRequest = ShippingRateRequest
  { srrEmail :: Text,
    srrFirstName :: Text,
    srrLastName :: Text,
    srrAddressLine1 :: Text,
    srrAddressLine2 :: Text,
    srrCity :: Text,
    srrState :: Text,
    srrZip :: Text,
    srrCart :: [CartItem]
  }
  deriving stock (Show, Generic)

instance FromJSON ShippingRateRequest where
  parseJSON = genericParseJSON (prefixOptions "srr")

--------------------------------------------------------------------------------

-- | Request to create a Stripe checkout session.
--
-- JSON fields: @email@, @first_name@, ..., @shipping_rate_id@, @shipping_shipment_id@, @cart@
data CreateSessionRequest = CreateSessionRequest
  { csrEmail :: Text,
    csrFirstName :: Text,
    csrLastName :: Text,
    csrAddressLine1 :: Text,
    csrAddressLine2 :: Text,
    csrCity :: Text,
    csrState :: Text,
    csrZip :: Text,
    csrShippingRateId :: Text,
    csrShippingShipmentId :: Text,
    csrCart :: [CartItem]
  }
  deriving stock (Show, Generic)

instance FromJSON CreateSessionRequest where
  parseJSON = genericParseJSON (prefixOptions "csr")

--------------------------------------------------------------------------------

-- | Response from create-session (JSON returned to the client).
--
-- JSON fields: @client_secret@, @order_number@
data CreateSessionResponse = CreateSessionResponse
  { csrespClientSecret :: Text,
    csrespOrderNumber :: Text
  }
  deriving stock (Show, Generic)

instance ToJSON CreateSessionResponse where
  toJSON = genericToJSON (prefixOptions "csresp")

--------------------------------------------------------------------------------

-- | Aeson options that strip a record field prefix and convert to snake_case.
--
-- @prefixOptions "srr"@ maps @"srrFirstName"@ to @"first_name"@,
-- @"srrAddressLine1"@ to @"address_line1"@, etc.
prefixOptions :: String -> Aeson.Options
prefixOptions prefix =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . fromJust . stripPrefix prefix
    }
