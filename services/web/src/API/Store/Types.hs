module API.Store.Types
  ( CartItem (..),
    CartRequest,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
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
