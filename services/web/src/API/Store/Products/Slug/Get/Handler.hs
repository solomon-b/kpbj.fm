{-# LANGUAGE OverloadedRecordDot #-}

module API.Store.Products.Slug.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Store.Products.Slug.Get.Templates (template)
import API.Types (Routes (..))
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Functor ((<&>))
import Data.Has (getter)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.HxRequest qualified as HxRequest
import Domain.Types.Slug (Slug (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ProductImages qualified as ProductImages
import Effects.Database.Tables.ProductOptionTypes qualified as ProductOptionTypes
import Effects.Database.Tables.ProductOptionValues qualified as ProductOptionValues
import Effects.Database.Tables.ProductVariantOptions qualified as ProductVariantOptions
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the product detail page.
data ProductDetailData = ProductDetailData
  { pddStorageBackend :: StorageBackend,
    pddProduct :: Products.Model,
    pddImages :: [ProductImages.Model],
    pddOptionTypes :: [ProductOptionTypes.Model],
    pddOptionValues :: [ProductOptionValues.Model],
    pddVariants :: [ProductVariants.Model],
    pddVariantOptions :: [ProductVariantOptions.VariantOption]
  }

--------------------------------------------------------------------------------

-- | Handler for the public product detail page.
--
-- Fetches a product by slug, verifies it is active, loads all related
-- data (images, option types, option values, variants, variant-options),
-- and renders the detail template.
handler ::
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Slug slugText) cookie hxRequest =
  handleHtmlErrors "Product detail" apiLinks.rootGet $ do
    mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
    vd <- action slugText
    let hxReq = HxRequest.foldHxReq hxRequest
    lift $
      renderTemplate hxReq mUserInfo $
        template
          vd.pddStorageBackend
          vd.pddProduct
          vd.pddImages
          vd.pddOptionTypes
          vd.pddOptionValues
          vd.pddVariants
          vd.pddVariantOptions

--------------------------------------------------------------------------------

-- | Business logic: fetch product and all related data.
action :: Text -> ExceptT HandlerError AppM ProductDetailData
action slugText = do
  storageBackend <- asks getter

  -- Fetch product by slug; 404 if missing
  product' <-
    fromMaybeM (throwNotFound "Product") $
      fromRightM throwDatabaseError $
        execQuery (Products.getBySlug slugText)

  -- Inactive products are treated as not found
  unless product'.pIsActive $
    throwNotFound "Product"

  let productId = product'.pId

  -- Fetch all related data
  images <- fromRightM throwDatabaseError $ execQuery (ProductImages.getByProductId productId)
  optionTypes <- fromRightM throwDatabaseError $ execQuery (ProductOptionTypes.getByProductId productId)
  optionValues <- fromRightM throwDatabaseError $ execQuery (ProductOptionValues.getByProductId productId)
  variants <- fromRightM throwDatabaseError $ execQuery (ProductVariants.getByProductId productId)
  variantOptions <- fromRightM throwDatabaseError $ execQuery (ProductVariantOptions.getByProductId productId)

  pure
    ProductDetailData
      { pddStorageBackend = storageBackend,
        pddProduct = product',
        pddImages = images,
        pddOptionTypes = optionTypes,
        pddOptionValues = optionValues,
        pddVariants = variants,
        pddVariantOptions = variantOptions
      }
