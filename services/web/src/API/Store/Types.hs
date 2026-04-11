module API.Store.Types
  ( CartItem (..),
    CartRequest,
    ShippingRateRequest (..),
    CreateSessionRequest (..),
    CreateSessionResponse (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
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
  deriving anyclass (FromJSON)

--------------------------------------------------------------------------------

-- | Request to create a Stripe checkout session.
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
  deriving anyclass (FromJSON)

--------------------------------------------------------------------------------

-- | Response from create-session (JSON returned to the client).
data CreateSessionResponse = CreateSessionResponse
  { csrespClientSecret :: Text,
    csrespOrderNumber :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
