{-# LANGUAGE OverloadedRecordDot #-}

module API.Store.Cart.Validate.Post.Handler where

--------------------------------------------------------------------------------

import API.Store.Cart.Validate.Post.Templates (CartWarning (..), ValidatedCartItem (..))
import API.Store.Cart.Validate.Post.Templates qualified as Templates
import API.Store.Types (CartItem (..), CartRequest)
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Data.Has (getter)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.ProductImages qualified as ProductImages
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for cart validation.
--
-- Takes a list of cart items from the client, validates each against
-- current DB state (product exists, is active, variant exists and not
-- deleted, inventory available), adjusts quantities as needed, and
-- returns a raw HTML fragment for the cart page.
handler ::
  CartRequest ->
  AppM (Lucid.Html ())
handler cart = do
  storageBackend <- asks getter
  -- Cap cart size to prevent unbounded DB queries from crafted requests
  let cart' = take 50 cart
  (validatedItems, warnings) <- validateCartItems cart'
  pure $ Templates.template storageBackend validatedItems warnings

--------------------------------------------------------------------------------

-- | Validate all cart items against current database state.
--
-- For each item:
--   1. Fetch the product by ID; skip if missing or inactive
--   2. If a variant ID is present, fetch it; skip if missing or deleted
--   3. Determine the effective price and available inventory
--   4. Clamp quantity to available inventory; warn if adjusted
--   5. Fetch the hero image for the product
validateCartItems ::
  CartRequest ->
  AppM ([ValidatedCartItem], [CartWarning])
validateCartItems cart = do
  results <- traverse validateOne cart
  let (items, warnings) = foldr collect ([], []) results
  pure (items, warnings)
  where
    collect ::
      (Maybe ValidatedCartItem, [CartWarning]) ->
      ([ValidatedCartItem], [CartWarning]) ->
      ([ValidatedCartItem], [CartWarning])
    collect (mItem, ws) (accItems, accWarnings) =
      ( maybe accItems (: accItems) mItem,
        ws <> accWarnings
      )

--------------------------------------------------------------------------------

-- | Validate a single cart item.
--
-- Returns 'Nothing' for the item if it should be removed from the cart
-- (product missing, inactive, variant deleted, or zero inventory).
-- Warnings are generated for any adjustments.
validateOne :: CartItem -> AppM (Maybe ValidatedCartItem, [CartWarning])
validateOne cartItem
  | cartItem.quantity <= 0 =
      pure (Nothing, [CartWarning "Invalid quantity" "Item removed (invalid quantity)."])
  | otherwise = do
      mProduct <- execQueryThrow $ Products.getById cartItem.productId
      case mProduct of
        Nothing -> do
          Log.logInfo "Cart validate: product not found" (show cartItem.productId)
          pure (Nothing, [CartWarning "Unknown product" "This product is no longer available and was removed from your cart."])
        Just product' -> validateProduct product' cartItem

-- | Validate a cart item given its resolved product.
validateProduct :: Products.Model -> CartItem -> AppM (Maybe ValidatedCartItem, [CartWarning])
validateProduct product' cartItem = do
  if not product'.pIsActive
    then do
      Log.logInfo "Cart validate: product inactive" (show product'.pId)
      pure
        ( Nothing,
          [CartWarning product'.pName "This product is no longer available and was removed from your cart."]
        )
    else case cartItem.variantId of
      Nothing -> validateSimpleProduct product' cartItem
      Just vid -> validateVariantProduct product' vid cartItem

-- | Validate a cart item for a product without variants.
validateSimpleProduct :: Products.Model -> CartItem -> AppM (Maybe ValidatedCartItem, [CartWarning])
validateSimpleProduct product' cartItem = do
  heroPath <- fetchHeroImagePath product'.pId
  let available = product'.pInventoryCount
  if available <= 0
    then
      pure
        ( Nothing,
          [CartWarning product'.pName "This product is out of stock and was removed from your cart."]
        )
    else do
      let (qty, warnings) = clampQuantity product'.pName cartItem.quantity (fromIntegral available)
      pure
        ( Just
            ValidatedCartItem
              { vciProductId = product'.pId,
                vciVariantId = Nothing,
                vciProductName = product'.pName,
                vciVariantLabel = Nothing,
                vciUnitPriceCents = product'.pBasePriceCents,
                vciQuantity = qty,
                vciHeroImagePath = heroPath
              },
          warnings
        )

-- | Validate a cart item for a product with a specific variant.
validateVariantProduct :: Products.Model -> ProductVariants.Id -> CartItem -> AppM (Maybe ValidatedCartItem, [CartWarning])
validateVariantProduct product' variantId cartItem = do
  mVariant <- execQueryThrow $ ProductVariants.getById variantId
  case mVariant of
    Nothing -> do
      Log.logInfo "Cart validate: variant not found" (show variantId)
      pure
        ( Nothing,
          [CartWarning product'.pName "The selected option is no longer available and was removed from your cart."]
        )
    Just variant
      -- Verify variant belongs to this product
      | variant.pvProductId /= product'.pId -> do
          Log.logInfo "Cart validate: variant/product mismatch" (show variantId <> " / " <> show product'.pId)
          pure (Nothing, [CartWarning product'.pName "The selected option is no longer available and was removed from your cart."])
      -- Soft-deleted variants are treated as unavailable
      | isJust variant.pvDeletedAt ->
          pure
            ( Nothing,
              [CartWarning product'.pName ("The \"" <> variant.pvLabel <> "\" option is no longer available and was removed from your cart.")]
            )
      | otherwise -> do
          let available = variant.pvInventoryCount
              priceCents = fromMaybe product'.pBasePriceCents variant.pvPriceCents
          heroPath <- fetchHeroImagePath product'.pId

          if available <= 0
            then
              pure
                ( Nothing,
                  [CartWarning product'.pName ("The \"" <> variant.pvLabel <> "\" option is out of stock and was removed from your cart.")]
                )
            else do
              let (qty, warnings) = clampQuantity product'.pName cartItem.quantity (fromIntegral available)
              pure
                ( Just
                    ValidatedCartItem
                      { vciProductId = product'.pId,
                        vciVariantId = Just variantId,
                        vciProductName = product'.pName,
                        vciVariantLabel = Just variant.pvLabel,
                        vciUnitPriceCents = priceCents,
                        vciQuantity = qty,
                        vciHeroImagePath = heroPath
                      },
                  warnings
                )

--------------------------------------------------------------------------------
-- Helpers

-- | Clamp requested quantity to available inventory.
--
-- Returns the clamped quantity and any warning if adjustment was needed.
clampQuantity :: Text -> Int -> Int -> (Int, [CartWarning])
clampQuantity productName requested available
  | requested <= available = (requested, [])
  | otherwise =
      ( available,
        [ CartWarning
            productName
            ("Only " <> Text.pack (show available) <> " available. Quantity adjusted.")
        ]
      )

-- | Fetch the hero image path (first image by sort order) for a product.
fetchHeroImagePath :: Products.Id -> AppM (Maybe Text)
fetchHeroImagePath productId = do
  images <- execQueryThrow $ ProductImages.getByProductId productId
  pure $ (.piImagePath) <$> listToMaybe images
