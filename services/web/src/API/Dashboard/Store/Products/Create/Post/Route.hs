module API.Dashboard.Store.Products.Create.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..), parseMaybe, parseUnique)

--------------------------------------------------------------------------------

-- | "POST /dashboard/store/products"
--
-- Inline create: accepts minimal product info, returns new table row HTML.
type Route =
  "dashboard"
    :> "store"
    :> "products"
    :> Servant.Header "Cookie" Cookie
    :> Servant.ReqBody '[Servant.FormUrlEncoded] InlineCreateForm
    :> Servant.Post '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

-- | Form data for inline product creation.
data InlineCreateForm = InlineCreateForm
  { icfName :: Text,
    icfCategory :: Maybe Text,
    -- | Price in dollars as text (e.g. "24.99"); converted to cents in handler.
    icfBasePriceDollars :: Text,
    -- | Inventory count as text; defaults to 0.
    icfInventoryCount :: Text
  }
  deriving (Show)

instance FromForm InlineCreateForm where
  fromForm form = do
    name <- parseUnique "name" form
    category <- parseMaybe "category" form
    basePriceDollars <- parseUnique "base_price_dollars" form
    inventoryCount <- parseUnique "inventory_count" form
    pure
      InlineCreateForm
        { icfName = name,
          icfCategory = category,
          icfBasePriceDollars = basePriceDollars,
          icfInventoryCount = inventoryCount
        }
