{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Store.Products.Id.Edit.Get.Handler
  ( handler,
    action,
    ProductEditViewData (..),
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Products.Id.Edit.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ProductImages qualified as ProductImages
import Effects.Database.Tables.ProductOptionTypes qualified as ProductOptionTypes
import Effects.Database.Tables.ProductOptionValues qualified as ProductOptionValues
import Effects.Database.Tables.ProductVariantOptions qualified as ProductVariantOptions
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Lucid.Form.Builder (ImageData (..))
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the product edit form.
data ProductEditViewData = ProductEditViewData
  { pevUserMetadata :: UserMetadata.Model,
    pevAllShows :: [Shows.Model],
    pevSelectedShow :: Maybe Shows.Model,
    pevProduct :: Products.Model,
    pevImages :: [ImageData],
    pevOptionsVariantsJson :: Text,
    pevHasVariants :: Bool
  }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Products.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler productId cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Product edit form" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to edit products." userMetadata
    vd <- action user userMetadata productId
    lift $
      renderDashboardTemplate
        hxRequest
        vd.pevUserMetadata
        vd.pevAllShows
        vd.pevSelectedShow
        NavStoreProducts
        Nothing
        Nothing
        (template vd.pevUserMetadata vd.pevProduct vd.pevImages vd.pevHasVariants vd.pevOptionsVariantsJson)

--------------------------------------------------------------------------------

-- | Business logic: fetch product and all related data.
action ::
  User.Model ->
  UserMetadata.Model ->
  Products.Id ->
  ExceptT HandlerError AppM ProductEditViewData
action user userMetadata productId = do
  -- 1. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
      selectedShow = listToMaybe allShows

  -- 2. Fetch product
  productModel <- fromMaybeM (throwNotFound "Product") $ fromRightM throwDatabaseError $ execQuery (Products.getById productId)

  -- 3. Fetch images, option types, option values, variants, and variant-option join rows
  images <- fromRightM throwDatabaseError $ execQuery (ProductImages.getByProductId productId)
  optionTypes <- fromRightM throwDatabaseError $ execQuery (ProductOptionTypes.getByProductId productId)
  optionValues <- fromRightM throwDatabaseError $ execQuery (ProductOptionValues.getByProductId productId)
  variants <- fromRightM throwDatabaseError $ execQuery (ProductVariants.getByProductId productId)
  variantOptions <- fromRightM throwDatabaseError $ execQuery (ProductVariantOptions.getByProductId productId)

  Log.logInfo "Loading product edit form" productId

  storageBackend <- lift $ asks (Has.getter @StorageBackend)

  let optionsVariantsJson = buildOptionsVariantsJson optionTypes optionValues variants variantOptions
      imageDataList = map (toImageData storageBackend) images

  pure
    ProductEditViewData
      { pevUserMetadata = userMetadata,
        pevAllShows = allShows,
        pevSelectedShow = selectedShow,
        pevProduct = productModel,
        pevImages = imageDataList,
        pevOptionsVariantsJson = optionsVariantsJson,
        pevHasVariants = not (null variants)
      }

-- | Serialize options, variants, and their mappings as JSON for the Alpine.js component.
--
-- Produces a JSON object with:
--
-- - @options@: array of @{name, values}@ objects
-- - @variants@: array of @{id, option_values, price_cents, inventory_count, sku, weight_oz}@ objects
-- - @deletedVariantIds@: empty array (initial state)
buildOptionsVariantsJson ::
  [ProductOptionTypes.Model] ->
  [ProductOptionValues.Model] ->
  [ProductVariants.Model] ->
  [ProductVariantOptions.VariantOption] ->
  Text
buildOptionsVariantsJson optionTypes optionValues variants variantOptions =
  Text.decodeUtf8 $ BSL.toStrict $ Aeson.encode jsonObj
  where
    -- Pre-build lookup maps for O(1) access
    valuesByType :: Map.Map ProductOptionTypes.Id [ProductOptionValues.Model]
    valuesByType = Map.fromListWith (<>) [(v.povOptionTypeId, [v]) | v <- optionValues]

    valueById :: Map.Map ProductOptionValues.Id Text
    valueById = Map.fromList [(v.povId, v.povValue) | v <- optionValues]

    optValueIdsByVariant :: Map.Map ProductVariants.Id [ProductOptionValues.Id]
    optValueIdsByVariant = Map.fromListWith (<>) [(vo.voVariantId, [vo.voOptionValueId]) | vo <- variantOptions]

    jsonObj =
      Aeson.object
        [ "options" Aeson..= map buildOption optionTypes,
          "variants" Aeson..= map buildVariant variants,
          "deletedVariantIds" Aeson..= ([] :: [Int])
        ]

    buildOption ot =
      let valuesForType = fromMaybe [] $ Map.lookup ot.potId valuesByType
       in Aeson.object
            [ "name" Aeson..= ot.potName,
              "values" Aeson..= map (.povValue) valuesForType
            ]

    buildVariant v =
      let variantOptValueIds = fromMaybe [] $ Map.lookup v.pvId optValueIdsByVariant
          variantOptValues = mapMaybe (`Map.lookup` valueById) variantOptValueIds
       in Aeson.object
            [ "id" Aeson..= v.pvId,
              "option_values" Aeson..= variantOptValues,
              "price_cents" Aeson..= v.pvPriceCents,
              "inventory_count" Aeson..= v.pvInventoryCount,
              "sku" Aeson..= v.pvSku,
              "weight_oz" Aeson..= v.pvWeightOz
            ]

-- | Convert a database image record to the form builder's 'ImageData' type.
toImageData :: StorageBackend -> ProductImages.Model -> ImageData
toImageData backend img =
  ImageData
    { imgId = Just (ProductImages.unId img.piId),
      imgUrl = buildMediaUrl backend img.piImagePath,
      imgAltText = img.piAltText
    }
