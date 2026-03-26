module API.Dashboard.Store.Products.Id.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.Products qualified as Products
import Servant ((:>))
import Servant qualified
import Servant.Multipart (FileData (..), FromMultipart, Mem, MultipartData (..), MultipartForm, fromMultipart, lookupInput)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/store/products/:productId/edit"
type Route =
  "dashboard"
    :> "store"
    :> "products"
    :> Servant.Capture "productId" Products.Id
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> MultipartForm Mem EditProductForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)

--------------------------------------------------------------------------------

-- | Form data for editing an existing product's basic info.
--
-- Numeric fields are received as Text and parsed in the handler.
-- The @is_active@ field is @Just "on"@ when the checkbox is checked and
-- @Nothing@ when it is unchecked (browsers omit unchecked checkboxes).
-- The slug is immutable after creation, so it is not included here.
data EditProductForm = EditProductForm
  { epfName :: Text,
    epfDescription :: Text,
    -- | Price in dollars as text (e.g. "24.99"); converted to cents in handler.
    epfBasePriceDollars :: Text,
    -- | Weight in ounces as text.
    epfWeightOz :: Text,
    -- | Optional product category.
    epfCategory :: Maybe Text,
    -- | "on" when active checkbox is ticked; Nothing when unticked.
    epfIsActive :: Maybe Text,
    -- | Sort order as text; defaults to "0".
    epfSortOrder :: Text,
    -- | Inventory count as text; for products without variants.
    -- Absent when the field is disabled (variants exist).
    epfInventoryCount :: Maybe Text,
    -- | JSON payload for options + variants. Absent when product has no options.
    epfVariantsJson :: Maybe Text,
    -- | JSON metadata for images: [{id, sort_order, alt_text}]
    epfImagesData :: Maybe Text,
    -- | JSON array of deleted image IDs
    epfImagesDeleted :: Maybe Text,
    -- | New image files from the imagesField
    epfImageFiles :: [FileData Mem]
  }
  deriving (Show)

instance FromMultipart Mem EditProductForm where
  fromMultipart multipartData = do
    name <- lookupInput "name" multipartData
    let lookupOpt key = either (const Nothing) Just (lookupInput key multipartData)
        description = fromMaybe "" (lookupOpt "description")
        basePriceDollars = fromMaybe "" (lookupOpt "base_price_dollars")
        weightOz = fromMaybe "" (lookupOpt "weight_oz")
        category = lookupOpt "category"
        isActive = lookupOpt "is_active"
        sortOrder = fromMaybe "0" (lookupOpt "sort_order")
        inventoryCount = lookupOpt "inventory_count"
        variantsJson = lookupOpt "variants_json"
        imagesData = lookupOpt "product_images_data"
        imagesDeleted = lookupOpt "product_images_deleted"
        imageFiles = getAllFiles "product_images_files" multipartData
    pure
      EditProductForm
        { epfName = name,
          epfDescription = description,
          epfBasePriceDollars = basePriceDollars,
          epfWeightOz = weightOz,
          epfCategory = category,
          epfIsActive = isActive,
          epfSortOrder = sortOrder,
          epfInventoryCount = inventoryCount,
          epfVariantsJson = variantsJson,
          epfImagesData = imagesData,
          epfImagesDeleted = imagesDeleted,
          epfImageFiles = imageFiles
        }

-- | Extract all files with a given input name from multipart data.
--
-- 'lookupFile' only returns the first match; this helper returns all files
-- matching the specified field name (needed for multi-file uploads).
getAllFiles :: Text -> MultipartData Mem -> [FileData Mem]
getAllFiles fieldName md = filter (\fd -> fdInputName fd == fieldName) (files md)
