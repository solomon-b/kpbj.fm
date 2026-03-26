{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Store.Products.Id.Edit.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Products.Id.Edit.Post.Route (EditProductForm (..))
import API.Links (apiLinks, dashboardStoreProductsLinks, rootLink)
import API.Types (DashboardStoreProductsRoutes (..), Routes (..))
import App.Handler.Combinators (requireAuth, requireRight, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad (foldM, forM, forM_, unless, void, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (FromJSON (..), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.Has (getter)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Domain.Types.Cents qualified as Cents
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.VariantsPayload (OptionPayload (..), VariantPayload (..), VariantsPayload (..))
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.ProductImages qualified as ProductImages
import Effects.Database.Tables.ProductOptionTypes qualified as ProductOptionTypes
import Effects.Database.Tables.ProductOptionValues qualified as ProductOptionValues
import Effects.Database.Tables.ProductVariantOptions qualified as ProductVariantOptions
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (uploadProductImage)
import Effects.StagedUploadCleanup (deleteFile)
import Hasql.Transaction qualified as HT
import Log qualified
import Servant qualified
import Servant.Multipart (FileData, Mem)
import Text.Read (readMaybe)
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + building redirect response.
handler ::
  Products.Id ->
  Maybe Cookie ->
  EditProductForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler productId cookie editForm =
  handleRedirectErrors "Product update" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to edit products." userMetadata

    vd <- action user userMetadata productId editForm
    pure $ Servant.addHeader vd.eprRedirectUrl $ Servant.addHeader (flashCookie (Just vd.eprFlash)) Servant.NoContent

--------------------------------------------------------------------------------
-- Validation

parseNonNull :: Text -> Maybe Text
parseNonNull txt =
  if Text.null (Text.strip txt) then Nothing else Just txt

-- | Strip whitespace if the description is otherwise empty.
parseDescription :: Text -> Text
parseDescription = fold . parseNonNull

parsePrice :: Text -> Either Text Cents.Cents
parsePrice dollars =
  case readMaybe @Scientific (Text.unpack dollars) of
    Just d | d >= 0 -> Right (Cents.dollarsToCents d)
    _ -> Left ("Invalid price: \"" <> dollars <> "\" is not a valid amount")

-- | Validate the edit product form, returning a Products.Insert or an error.
--
-- The slug is preserved from the existing product (immutable after creation).
validateEditForm ::
  -- | Existing product slug to preserve.
  Text ->
  EditProductForm ->
  Either Text Products.Insert
validateEditForm existingSlug form = do
  -- Sanitize and validate name
  let sanitizedName = Sanitize.sanitizeTitle form.epfName
  validName <- first Sanitize.displayContentValidationError $ Sanitize.validateContentLength 200 sanitizedName

  -- Sanitize description (empty allowed)
  let sanitizedDescription = Sanitize.sanitizeUserContent form.epfDescription
      description = parseDescription sanitizedDescription

  -- Parse price
  basePriceCents <- parsePrice form.epfBasePriceDollars

  -- Parse weight, inventory count, and sort order (0 if blank/invalid)
  let weightOz = Sanitize.parseIntDefault0 form.epfWeightOz
      -- Absent when field is disabled (variants exist) — default to 0
      inventoryCount = maybe 0 Sanitize.parseIntDefault0 form.epfInventoryCount
      sortOrder = Sanitize.parseIntDefault0 form.epfSortOrder

  -- Sanitize optional category
  let mCategory = form.epfCategory >>= parseNonNull . Sanitize.sanitizePlainText

  -- Parse is_active: "on" means True, absent means False
  let isActive = form.epfIsActive == Just "on"

  Right
    Products.Insert
      { Products.piName = validName,
        Products.piSlug = existingSlug,
        Products.piDescription = description,
        Products.piBasePriceCents = basePriceCents,
        Products.piWeightOz = weightOz,
        Products.piCategory = mCategory,
        Products.piInventoryCount = inventoryCount,
        Products.piIsActive = isActive,
        Products.piSortOrder = sortOrder
      }

--------------------------------------------------------------------------------

-- | All data needed to build the post-update redirect.
data EditProductRedirectData = EditProductRedirectData
  { eprRedirectUrl :: Text,
    eprFlash :: FlashMessage
  }

-- | Business logic: fetch product, validate form, update, build redirect data.
--
-- All preparation (validation, file uploads) happens first, then all DB
-- mutations run in a single transaction so a failure in variants or images
-- does not leave a partially-updated product.
action ::
  User.Model ->
  UserMetadata.Model ->
  Products.Id ->
  EditProductForm ->
  ExceptT HandlerError AppM EditProductRedirectData
action _user _userMetadata productId editForm = do
  -- 1. Verify the product exists
  mProduct <- fromRightM throwDatabaseError $ execQuery (Products.getById productId)
  existingProduct <- case mProduct of
    Nothing -> throwNotFound "Product"
    Just p -> pure p

  -- 2. Validate form data (preserve existing slug)
  productInsert <- requireRight id (validateEditForm existingProduct.pSlug editForm)

  -- 3. Prepare variants (parse, sanitize, validate — no DB writes)
  mVariantsPayload <- case editForm.epfVariantsJson of
    Nothing -> pure Nothing
    Just json -> Just <$> prepareVariants json

  -- 4. Prepare images (validate deletions, upload new files — no DB writes)
  (failedUploads, imagesToDelete, imageIdsToDelete, metaUpdates, newInserts) <-
    prepareImageUpdates productId existingProduct.pSlug editForm

  -- 5. Run ALL DB mutations in a single transaction
  fromRightM throwDatabaseError $
    execTransaction $ do
      void $ HT.statement () (Products.updateProduct productId productInsert)
      forM_ mVariantsPayload (variantsTransaction productId)
      imageTransaction productId imageIdsToDelete metaUpdates newInserts

  Log.logInfo "Successfully updated product" productId

  -- 6. Delete old image files from storage (after transaction succeeded)
  forM_ imagesToDelete $ \(_imageId, imagePath) -> do
    storageBackend <- lift $ asks getter
    mAwsEnv <- lift $ asks getter
    deleted <- lift $ deleteFile storageBackend mAwsEnv imagePath
    if deleted
      then Log.logInfo "Deleted product image file" imagePath
      else Log.logInfo "Failed to delete product image file (may not exist)" imagePath

  -- Redirect back to the edit page with a flash
  let redirectUrl = rootLink $ dashboardStoreProductsLinks.editGet productId
      flash
        | failedUploads > 0 =
            FlashMessage Warning "Product Updated" $
              "Product saved, but " <> Text.pack (show failedUploads) <> " image(s) failed to upload."
        | otherwise =
            FlashMessage Success "Product Updated" "Product details have been saved."

  pure $ EditProductRedirectData redirectUrl flash

--------------------------------------------------------------------------------
-- Image handling

-- | JSON metadata for a single image from the frontend imagesField component.
data ImageMetadata = ImageMetadata
  { imId :: Maybe Int64,
    imSortOrder :: Int64,
    imAltText :: Text
  }

instance FromJSON ImageMetadata where
  parseJSON = Aeson.withObject "ImageMetadata" $ \o ->
    ImageMetadata
      <$> o .:? "id"
      <*> o .: "sort_order"
      <*> o .: "alt_text"

-- | Prepare image updates: validate deletions, upload new files.
--
-- No DB writes happen here. Returns all data needed for 'imageTransaction'
-- and post-transaction file cleanup.
prepareImageUpdates ::
  Products.Id ->
  -- | Product slug for generating filenames.
  Text ->
  EditProductForm ->
  ExceptT HandlerError AppM (Int, [(ProductImages.Id, Text)], [ProductImages.Id], [(ProductImages.Id, Int64, Text)], [ProductImages.Insert])
prepareImageUpdates productId productSlug editForm = do
  -- 1. Validate deletion list and collect paths to delete (no file I/O yet)
  imagesToDelete <- case editForm.epfImagesDeleted of
    Nothing -> pure []
    Just deletedJson -> do
      deletedIds <- case Aeson.eitherDecodeStrict (Text.Encoding.encodeUtf8 deletedJson) of
        Left err -> throwValidationError ("Invalid deleted images JSON: " <> Text.pack err)
        Right ids -> pure (ids :: [Int64])
      fmap catMaybes $ forM deletedIds $ \rawId -> do
        let imageId = ProductImages.Id rawId
        mImage <- fromRightM throwDatabaseError $ execQuery (ProductImages.getById imageId)
        case mImage of
          Nothing -> pure Nothing
          Just image
            | image.piProductId /= productId -> do
                Log.logInfo "Skipping image deletion: image does not belong to product" (Aeson.object ["imageId" Aeson..= rawId, "imageProductId" Aeson..= image.piProductId, "requestProductId" Aeson..= productId])
                pure Nothing
            | otherwise ->
                pure (Just (imageId, image.piImagePath))

  -- 2. Upload new files (I/O — orphan files are harmless if transaction fails)
  (failedUploads, metaUpdates, newInserts) <- case editForm.epfImagesData of
    Nothing -> pure (0, [], [])
    Just imagesJson -> do
      imageMetaList <- case Aeson.eitherDecodeStrict (Text.Encoding.encodeUtf8 imagesJson) of
        Left err -> throwValidationError ("Invalid images metadata JSON: " <> Text.pack err)
        Right metas -> pure (metas :: [ImageMetadata])
      prepareImageOps productId productSlug imageMetaList editForm.epfImageFiles

  let imageIdsToDelete = map fst imagesToDelete
  pure (failedUploads, imagesToDelete, imageIdsToDelete, metaUpdates, newInserts)

-- | Prepare image operations by uploading new files, returning DB-ready data.
--
-- Separates metadata into updates (existing images) and inserts (new images
-- with freshly uploaded files). File uploads happen here; DB writes are deferred
-- to the transaction.
prepareImageOps ::
  Products.Id ->
  -- | Product slug for filenames.
  Text ->
  [ImageMetadata] ->
  -- | New image files (consumed in order for entries without an id).
  [FileData Mem] ->
  ExceptT HandlerError AppM (Int, [(ProductImages.Id, Int64, Text)], [ProductImages.Insert])
prepareImageOps productId productSlug metaList newFiles = do
  (remainingFiles, failCount, updates, inserts) <- foldM go (newFiles, 0, [], []) metaList
  -- Warn about leftover files
  unless (null remainingFiles) $
    Log.logInfo "Unused image files after processing metadata" (length remainingFiles)
  pure (failCount, reverse updates, reverse inserts)
  where
    go (files, fails, updates, inserts) meta
      | Just existingId <- meta.imId =
          -- Existing image: collect metadata update
          pure (files, fails, (ProductImages.Id existingId, meta.imSortOrder, Sanitize.sanitizePlainText meta.imAltText) : updates, inserts)
    go ([], fails, updates, inserts) meta = do
      Log.logInfo "No file available for new image metadata entry" meta.imSortOrder
      pure ([], fails + 1, updates, inserts)
    go (fileData : restFiles, fails, updates, inserts) meta = do
      -- Upload new image file
      storageBackend <- lift $ asks getter
      mAwsEnv <- lift $ asks getter
      uploadResult <- lift $ uploadProductImage storageBackend mAwsEnv productSlug fileData
      case uploadResult of
        Left uploadError -> do
          Log.logInfo "Product image upload failed" (Aeson.object ["error" Aeson..= Text.pack (show uploadError)])
          pure (restFiles, fails + 1, updates, inserts)
        Right Nothing ->
          pure (restFiles, fails + 1, updates, inserts)
        Right (Just result) -> do
          let storagePath = Text.pack (uploadResultStoragePath result)
          Log.logInfo "Product image uploaded successfully" (Aeson.object ["path" Aeson..= storagePath])
          let ins =
                ProductImages.Insert
                  { ProductImages.iiProductId = productId,
                    ProductImages.iiImagePath = storagePath,
                    ProductImages.iiAltText = Sanitize.sanitizePlainText meta.imAltText,
                    ProductImages.iiSortOrder = meta.imSortOrder
                  }
          pure (restFiles, fails, updates, ins : inserts)

-- | Transaction that applies all image DB mutations atomically.
imageTransaction ::
  Products.Id ->
  [ProductImages.Id] ->
  [(ProductImages.Id, Int64, Text)] ->
  [ProductImages.Insert] ->
  HT.Transaction ()
imageTransaction _productId deleteIds metaUpdates newInserts = do
  -- Delete removed images
  forM_ deleteIds $ \imageId ->
    void $ HT.statement () (ProductImages.deleteImage imageId)
  -- Update existing image metadata
  forM_ metaUpdates $ \(imageId, sortOrder, altText) ->
    HT.statement () (ProductImages.updateImageMeta imageId sortOrder altText)
  -- Insert newly uploaded images
  forM_ newInserts $ \ins ->
    void $ HT.statement () (ProductImages.insertImage ins)

--------------------------------------------------------------------------------
-- Variants handling

-- | Parse, sanitize, and validate the variants JSON payload.
--
-- Returns the sanitized payload ready for 'variantsTransaction'.
-- No database writes happen here.
prepareVariants ::
  Text ->
  ExceptT HandlerError AppM VariantsPayload
prepareVariants variantsJson = do
  -- Parse the JSON payload
  payload <- case Aeson.eitherDecodeStrict (Text.Encoding.encodeUtf8 variantsJson) of
    Left err -> throwValidationError ("Invalid variants JSON: " <> Text.pack err)
    Right vp -> pure vp

  -- Sanitize all user-provided text in the payload
  let sanitizeOption opt =
        opt
          { opName = Sanitize.sanitizePlainText opt.opName,
            opValues = map Sanitize.sanitizePlainText opt.opValues
          }
      sanitizeVariant v =
        v
          { vplSku = fmap Sanitize.sanitizePlainText v.vplSku,
            vplOptionValues = map Sanitize.sanitizePlainText v.vplOptionValues
          }
      sanitizedPayload =
        payload
          { vpOptions = map sanitizeOption payload.vpOptions,
            vpVariants = map sanitizeVariant payload.vpVariants
          }

  -- Validate payload size limits
  when (length sanitizedPayload.vpOptions > 10) $
    throwValidationError "A product cannot have more than 10 option types"
  when (length sanitizedPayload.vpVariants > 200) $
    throwValidationError "A product cannot have more than 200 variants"

  -- Validate the sanitized payload
  forM_ sanitizedPayload.vpOptions $ \opt -> do
    when (Text.null (Text.strip opt.opName)) $
      throwValidationError "Option type name cannot be empty"
    when (null opt.opValues) $
      throwValidationError ("Option type \"" <> opt.opName <> "\" must have at least one value")
    when (length opt.opValues > 50) $
      throwValidationError ("Option type \"" <> opt.opName <> "\" cannot have more than 50 values")
    forM_ opt.opValues $ \val ->
      when (Text.null (Text.strip val)) $
        throwValidationError ("Option type \"" <> opt.opName <> "\" contains an empty value")

  forM_ sanitizedPayload.vpVariants $ \v -> do
    when (v.vplInventoryCount < 0) $
      throwValidationError "Variant inventory count cannot be negative"
    forM_ v.vplPriceCents $ \price ->
      when (price < 0) $
        throwValidationError "Variant price cannot be negative"
    forM_ v.vplWeightOz $ \weight ->
      when (weight < 0) $
        throwValidationError "Variant weight cannot be negative"

  pure sanitizedPayload

-- | Transaction that replaces all options and syncs variants for a product.
--
-- This is composed as a 'Hasql.Transaction.Transaction' so that all operations
-- succeed or fail atomically.
variantsTransaction ::
  Products.Id ->
  VariantsPayload ->
  HT.Transaction ()
variantsTransaction productId payload = do
  -- 1. Delete all existing option types (cascades to values + variant-option join rows)
  void $ HT.statement () (ProductOptionTypes.deleteByProductId productId)

  -- 2. Insert new option types and their values, building a map from
  --    (optionName, valueName) -> new ProductOptionValues.Id
  optionValueMap <- buildOptionValueMap productId payload.vpOptions

  -- 3. Upsert variants (update existing, insert new)
  let indexedVariants = zip [0 ..] payload.vpVariants
  forM_ indexedVariants (upsertVariant productId optionValueMap)

  -- 4. Soft-delete removed variants
  forM_ payload.vpDeletedVariantIds (HT.statement () . ProductVariants.softDeleteVariant)

-- | Insert option types and their values, returning a map from
-- @(optionTypeIndex, valueName)@ to the newly assigned 'ProductOptionValues.Id'.
--
-- Keyed by option type index to avoid collisions when two option types
-- share a value name (e.g. Fit: "Regular" and Cut: "Regular").
buildOptionValueMap ::
  Products.Id ->
  [OptionPayload] ->
  HT.Transaction (Map.Map (Int64, Text) ProductOptionValues.Id)
buildOptionValueMap productId options = do
  maps <- mapM insertOption (zip [0 ..] options)
  pure $ Map.unions maps
  where
    insertOption (optionIdx, option) = do
      optionTypeId <-
        HT.statement () $
          ProductOptionTypes.insertOptionType
            ProductOptionTypes.Insert
              { otiProductId = productId,
                otiName = option.opName,
                otiSortOrder = optionIdx
              }
      pairs <- mapM (insertValue optionIdx optionTypeId) (zip [0 ..] option.opValues)
      pure $ Map.fromList pairs

    insertValue optionIdx optionTypeId (sortIdx, valueName) = do
      valueId <-
        HT.statement () $
          ProductOptionValues.insertOptionValue
            ProductOptionValues.Insert
              { oviOptionTypeId = optionTypeId,
                oviValue = valueName,
                oviSortOrder = sortIdx
              }
      pure ((optionIdx, valueName), valueId)

-- | Update an existing variant or insert a new one, then create its
-- variant-option join rows.
upsertVariant ::
  Products.Id ->
  Map.Map (Int64, Text) ProductOptionValues.Id ->
  (Int64, VariantPayload) ->
  HT.Transaction ()
upsertVariant productId optionValueMap (sortIdx, variant) = do
  -- Build the label from option values (e.g. "M / Red")
  let label = Text.intercalate " / " variant.vplOptionValues
      variantInsert =
        ProductVariants.Insert
          { viProductId = productId,
            viLabel = label,
            viPriceCents = variant.vplPriceCents,
            viInventoryCount = variant.vplInventoryCount,
            viSku = variant.vplSku,
            viWeightOz = variant.vplWeightOz,
            viSortOrder = sortIdx
          }

  -- Get the variant ID (update existing or insert new)
  mVariantId <- case variant.vplId of
    Just existingId -> do
      -- Clear old variant-option join rows for this variant
      HT.statement () (ProductVariantOptions.deleteByVariantId existingId)
      -- Update the variant
      HT.statement () (ProductVariants.updateVariant existingId variantInsert)
    Nothing ->
      -- Insert new variant
      HT.statement () (ProductVariants.insertVariant variantInsert)

  -- Create variant-option join rows
  case mVariantId of
    Nothing -> pure ()
    Just variantId ->
      mapM_ (linkVariantToOption variantId) (zip [0 ..] variant.vplOptionValues)
  where
    linkVariantToOption variantId (optionIdx, valueName) =
      case Map.lookup (optionIdx, valueName) optionValueMap of
        -- NOTE: Lookup can fail if the client sends variant option values that
        -- don't match the declared option types. This is a no-op — the join row
        -- is simply not created. The variant label still contains the value text.
        Nothing -> pure ()
        Just optionValueId ->
          HT.statement () (ProductVariantOptions.insertVariantOption variantId optionValueId)
