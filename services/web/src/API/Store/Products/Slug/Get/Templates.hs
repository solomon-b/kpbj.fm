{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Store.Products.Slug.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (storeLinks)
import API.Types (StoreRoutes (..))
import Control.Monad (unless, when)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cents qualified as Cents
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.ProductImages qualified as ProductImages
import Effects.Database.Tables.ProductOptionTypes qualified as ProductOptionTypes
import Effects.Database.Tables.ProductOptionValues qualified as ProductOptionValues
import Effects.Database.Tables.ProductVariantOptions qualified as ProductVariantOptions
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Lucid qualified
import Lucid.Alpine
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------
-- URL helpers

storeListUrl :: Links.URI
storeListUrl = Links.linkURI storeLinks.list

--------------------------------------------------------------------------------

-- | Product detail page template.
--
-- Renders the full product page with image gallery, variant selectors,
-- quantity stepper, and add-to-cart button. All client-side interactivity
-- is powered by Alpine.js.
template ::
  StorageBackend ->
  Products.Model ->
  [ProductImages.Model] ->
  [ProductOptionTypes.Model] ->
  [ProductOptionValues.Model] ->
  [ProductVariants.Model] ->
  [ProductVariantOptions.VariantOption] ->
  Lucid.Html ()
template backend product' images optionTypes optionValues variants variantOptions = do
  Lucid.div_
    [ Lucid.class_ "max-w-4xl mx-auto",
      xData_ (alpineState backend product' images optionTypes optionValues variants variantOptions)
    ]
    $ do
      -- Breadcrumb
      renderBreadcrumb product'.pName

      -- Main content: image gallery + product info
      Lucid.div_
        [ class_ $ base ["flex", "flex-col", "md:flex-row", Tokens.gap8]
        ]
        $ do
          -- Left column: Image gallery
          renderImageGallery backend images

          -- Right column: Product info + actions
          Lucid.div_ [class_ $ base ["flex-1", "min-w-0"]] $ do
            renderProductInfo product'
            renderVariantSelectors optionTypes
            renderQuantityStepper
            renderAddToCartButton product'
            renderCategory product'.pCategory

--------------------------------------------------------------------------------
-- Breadcrumb

renderBreadcrumb :: Text -> Lucid.Html ()
renderBreadcrumb productName =
  Lucid.nav_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb6]] $ do
    Lucid.a_
      [ Lucid.href_ [i|/#{storeListUrl}|],
        hxGet_ [i|/#{storeListUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "hover:underline"
      ]
      "Store"
    Lucid.span_ [Lucid.class_ "mx-2"] "/"
    Lucid.span_ [] $ Lucid.toHtml productName

--------------------------------------------------------------------------------
-- Image Gallery

renderImageGallery :: StorageBackend -> [ProductImages.Model] -> Lucid.Html ()
renderImageGallery backend images' =
  Lucid.div_ [class_ $ base ["w-full", "md:w-1/2", "flex-shrink-0"]] $
    case images' of
      [] ->
        -- Placeholder when no images
        Lucid.div_
          [ class_ $ base ["aspect-square", "border", Tokens.borderMuted, Tokens.bgAlt, "flex", "items-center", "justify-center"]
          ]
          $ Lucid.span_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] "No image"
      (firstImage : rest) -> do
        -- Main image
        Lucid.div_
          [ class_ $ base ["aspect-square", "overflow-hidden", "border", Tokens.borderMuted, Tokens.mb4]
          ]
          $ Lucid.img_
            [ xBindSrc_ "images[selectedImageIndex].url",
              Lucid.src_ (buildMediaUrl backend firstImage.piImagePath),
              Lucid.alt_ firstImage.piAltText,
              Lucid.class_ "w-full h-full object-cover"
            ]

        -- Thumbnails (only if more than one image)
        unless (null rest) $
          Lucid.div_ [class_ $ base ["flex", Tokens.gap2, "overflow-x-auto"]] $ do
            Lucid.template_ [xFor_ "(img, idx) in images"]
              $ Lucid.button_
                [ xOnClick_ "selectedImageIndex = idx",
                  xBindClass_ "idx === selectedImageIndex ? 'border-2 border-[var(--theme-fg-primary)] opacity-100' : 'border border-[var(--theme-border-muted)] opacity-60 hover:opacity-100'",
                  Lucid.class_ "w-16 h-16 overflow-hidden flex-shrink-0 transition-opacity"
                ]
              $ Lucid.img_
                [ xBindSrc_ "img.url",
                  Lucid.class_ "w-full h-full object-cover"
                ]

--------------------------------------------------------------------------------
-- Product Info

renderProductInfo :: Products.Model -> Lucid.Html ()
renderProductInfo product' = do
  let inventoryCount = product'.pInventoryCount

  -- Product name
  Lucid.h1_ [class_ $ base [Tokens.heading2xl, Tokens.mb4]] $
    Lucid.toHtml product'.pName

  -- Price (reactive when variants change the price)
  Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] $ do
    Lucid.span_ [xText_ "formatPrice(displayPrice())"] $
      Lucid.toHtml (Cents.formatDisplay product'.pBasePriceCents)

  -- Description
  when (product'.pDescription /= "") $
    Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb6, "whitespace-pre-wrap"]] $
      Lucid.toHtml product'.pDescription

  -- Inventory status
  Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.mb6]] $ do
    Lucid.template_ [xIf_ "optionTypes.length === 0"] $
      Lucid.div_ [] $ do
        Lucid.template_ [xIf_ [i|#{inventoryCount} > 0|]] $
          Lucid.span_ [class_ $ base [Tokens.fgMuted]] $
            Lucid.toHtml ([i|#{inventoryCount} in stock|] :: Text)
        Lucid.template_ [xIf_ [i|#{inventoryCount} <= 0|]] $
          Lucid.span_ [class_ $ base [Tokens.fontBold, "text-red-600"]] "OUT OF STOCK"
    Lucid.template_ [xIf_ "optionTypes.length > 0"] $
      Lucid.div_ [] $ do
        Lucid.template_ [xIf_ "resolvedVariant() && resolvedVariant().inventoryCount > 0"] $
          Lucid.span_ [class_ $ base [Tokens.fgMuted], xText_ "displayInventory() + ' in stock'"] ""
        Lucid.template_ [xIf_ "resolvedVariant() && resolvedVariant().inventoryCount <= 0"] $
          Lucid.span_ [class_ $ base [Tokens.fontBold, "text-red-600"]] "OUT OF STOCK"
        Lucid.template_ [xIf_ "!resolvedVariant()"] $
          Lucid.span_ [class_ $ base [Tokens.fgMuted]] "Select options to see availability"

--------------------------------------------------------------------------------
-- Variant Selectors

renderVariantSelectors :: [ProductOptionTypes.Model] -> Lucid.Html ()
renderVariantSelectors optionTypes' =
  unless (null optionTypes') $
    Lucid.div_ [class_ $ base [Tokens.mb6, "space-y-4"]] $
      Lucid.template_ [xFor_ "optType in optionTypes"] $
        Lucid.div_ [] $ do
          Lucid.label_
            [ Lucid.class_ Tokens.fontBold,
              xText_ "optType.name"
            ]
            ""
          Lucid.select_
            [ class_ $ base [Tokens.fullWidth, Tokens.p2, "border", Tokens.borderMuted, Tokens.bgAlt, "mt-1", "appearance-none", "cursor-pointer"],
              xModel_ "selectedOptions[optType.id]",
              xOnChange_ "quantity = 1"
            ]
            $ do
              Lucid.option_ [Lucid.value_ ""] "Select..."
              Lucid.template_ [xFor_ "val in optType.values"]
                $ Lucid.option_
                  [ xBindValue_ "val.id",
                    xBindDisabled_ "!isValueAvailable(optType.id, val.id)"
                  ]
                $ Lucid.span_ [xText_ "val.value + (!isValueAvailable(optType.id, val.id) ? ' (unavailable)' : '')"] ""

--------------------------------------------------------------------------------
-- Quantity Stepper

renderQuantityStepper :: Lucid.Html ()
renderQuantityStepper =
  Lucid.div_ [class_ $ base [Tokens.mb6]] $ do
    Lucid.label_ [class_ $ base [Tokens.fontBold, Tokens.mb2, "block"]] "Quantity"
    Lucid.div_ [class_ $ base ["inline-flex", "items-center", "border", Tokens.borderMuted]] $ do
      -- Decrement button
      Lucid.button_
        [ Lucid.type_ "button",
          class_ $ base [Tokens.px4, Tokens.py2, Tokens.fontBold, "select-none"],
          xOnClick_ "if (quantity > 1) quantity--",
          xBindDisabled_ "quantity <= 1",
          xBindClass_ "quantity <= 1 ? 'opacity-30 cursor-not-allowed' : 'hover:bg-[var(--theme-bg-alt)] cursor-pointer'"
        ]
        "\x2212"
      -- Count display
      Lucid.span_
        [ class_ $ base [Tokens.px4, Tokens.py2, "min-w-[3rem]", "text-center", "border-x", Tokens.borderMuted],
          xText_ "quantity"
        ]
        "1"
      -- Increment button
      Lucid.button_
        [ Lucid.type_ "button",
          class_ $ base [Tokens.px4, Tokens.py2, Tokens.fontBold, "select-none"],
          xOnClick_ "quantity++",
          xBindClass_ "'hover:bg-[var(--theme-bg-alt)] cursor-pointer'"
        ]
        "+"

--------------------------------------------------------------------------------
-- Add to Cart Button

renderAddToCartButton :: Products.Model -> Lucid.Html ()
renderAddToCartButton product' = do
  let productId = Products.unId product'.pId
  Lucid.div_ [class_ $ base [Tokens.mb6]] $ do
    Lucid.button_
      [ Lucid.type_ "button",
        class_ $ base [Tokens.fullWidth, Tokens.buttonPrimary, "text-center", "transition-colors"],
        xOnClick_ [i|addToCart(#{productId})|],
        xBindDisabled_ "!canAddToCart()",
        xBindClass_ "!canAddToCart() ? 'opacity-40 cursor-not-allowed' : 'cursor-pointer'"
      ]
      $ Lucid.span_ [xText_ "added ? 'ADDED \\u2713' : 'ADD TO CART'"] "ADD TO CART"

--------------------------------------------------------------------------------
-- Category

renderCategory :: Maybe Text -> Lucid.Html ()
renderCategory mCategory =
  for_ mCategory $ \category ->
    Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, "border-t", Tokens.borderMuted, Tokens.py4]] $ do
      Lucid.span_ [Lucid.class_ Tokens.fontBold] "Category: "
      Lucid.toHtml category

--------------------------------------------------------------------------------
-- Alpine.js State

-- | Build the Alpine.js state object for the product detail component.
--
-- Serializes product data to JSON and embeds it alongside method definitions
-- in a JavaScript object literal via the @[i|...|]@ quasiquoter.
alpineState ::
  StorageBackend ->
  Products.Model ->
  [ProductImages.Model] ->
  [ProductOptionTypes.Model] ->
  [ProductOptionValues.Model] ->
  [ProductVariants.Model] ->
  [ProductVariantOptions.VariantOption] ->
  Text
alpineState backend product' images optionTypes optionValues variants variantOptions =
  let basePriceCents = Cents.unCents product'.pBasePriceCents
      inventoryCount = product'.pInventoryCount
   in [i|{
  images: #{imagesJson},
  selectedImageIndex: 0,
  optionTypes: #{optionTypesJson},
  variants: #{variantsJson},
  basePriceCents: #{basePriceCents},
  selectedOptions: {},
  quantity: 1,
  added: false,

  resolvedVariant() {
    const selected = Object.values(this.selectedOptions).map(Number);
    if (selected.length !== this.optionTypes.length) return null;
    return this.variants.find(v =>
      selected.every(ovId => v.optionValueIds.includes(ovId))
    ) || null;
  },

  isValueAvailable(optTypeId, valId) {
    const numValId = Number(valId);
    return this.variants.some(v => {
      if (v.inventoryCount <= 0) return false;
      if (!v.optionValueIds.includes(numValId)) return false;
      for (const [otId, ovId] of Object.entries(this.selectedOptions)) {
        if (parseInt(otId) === optTypeId) continue;
        if (!v.optionValueIds.includes(Number(ovId))) return false;
      }
      return true;
    });
  },

  displayPrice() {
    const v = this.resolvedVariant();
    return (v && v.priceCents !== null) ? v.priceCents : this.basePriceCents;
  },

  displayInventory() {
    const v = this.resolvedVariant();
    return v ? v.inventoryCount : 0;
  },

  formatPrice(cents) {
    return '$' + (cents / 100).toFixed(2);
  },

  canAddToCart() {
    if (this.optionTypes.length > 0) {
      const v = this.resolvedVariant();
      return v && v.inventoryCount > 0;
    }
    return #{inventoryCount} > 0;
  },

  addToCart(productId) {
    const variantId = this.resolvedVariant() ? this.resolvedVariant().id : null;
    Alpine.store('cart').addItem(productId, variantId, this.quantity);
    this.added = true;
    setTimeout(() => { this.added = false; }, 1500);
  }
}|]
  where
    imagesJson = encodeToText $ map buildImageJson images
    optionTypesJson = encodeToText $ map buildOptionTypeJson optionTypes
    variantsJson = encodeToText $ map buildVariantJson variants

    -- Group option values by their option type
    valuesByType :: Map.Map ProductOptionTypes.Id [ProductOptionValues.Model]
    valuesByType = Map.fromListWith (flip (<>)) [(v.povOptionTypeId, [v]) | v <- optionValues]

    -- Map variant IDs to their associated option value IDs
    optValueIdsByVariant :: Map.Map ProductVariants.Id [Int64]
    optValueIdsByVariant =
      Map.fromListWith
        (<>)
        [ (vo.voVariantId, [ProductOptionValues.unId vo.voOptionValueId])
        | vo <- variantOptions
        ]

    buildImageJson :: ProductImages.Model -> Aeson.Value
    buildImageJson img =
      Aeson.object
        [ "url" .= buildMediaUrl backend img.piImagePath,
          "alt" .= img.piAltText
        ]

    buildOptionTypeJson :: ProductOptionTypes.Model -> Aeson.Value
    buildOptionTypeJson ot =
      let vals = fromMaybe [] $ Map.lookup ot.potId valuesByType
       in Aeson.object
            [ "id" .= ot.potId,
              "name" .= ot.potName,
              "values" .= map buildOptionValueJson vals
            ]

    buildOptionValueJson :: ProductOptionValues.Model -> Aeson.Value
    buildOptionValueJson ov =
      Aeson.object
        [ "id" .= ov.povId,
          "value" .= ov.povValue
        ]

    buildVariantJson :: ProductVariants.Model -> Aeson.Value
    buildVariantJson v =
      let ovIds = fromMaybe [] $ Map.lookup v.pvId optValueIdsByVariant
       in Aeson.object
            [ "id" .= v.pvId,
              "priceCents" .= v.pvPriceCents,
              "inventoryCount" .= v.pvInventoryCount,
              "optionValueIds" .= ovIds
            ]

-- | Encode a value to a JSON 'Text' string.
encodeToText :: (Aeson.ToJSON a) => a -> Text
encodeToText = Text.decodeUtf8 . BSL.toStrict . Aeson.encode
