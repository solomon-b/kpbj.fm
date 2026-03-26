{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Store.Products.Id.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStoreProductsLinks, rootLink)
import API.Types (DashboardStoreProductsRoutes (..))
import Data.Int (Int64)
import Data.Scientific (FPFormat (..), formatScientific)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cents qualified as Cents
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Alpine
import Lucid.Base (makeAttributes)
import Lucid.Form.Builder
import Lucid.HTMX

--------------------------------------------------------------------------------
-- URL helpers

productsListUrl :: Text
productsListUrl = rootLink dashboardStoreProductsLinks.list

productEditPostUrl :: Products.Id -> Text
productEditPostUrl pid = rootLink $ dashboardStoreProductsLinks.editPost pid

--------------------------------------------------------------------------------

-- | Product edit page template with three sections plus Alpine.js options/variants component.
template ::
  UserMetadata.Model ->
  Products.Model ->
  -- | Existing images for the imagesField
  [ImageData] ->
  -- | Whether the product currently has variants
  Bool ->
  -- | JSON string for options/variants Alpine.js state
  Text ->
  Lucid.Html ()
template userMeta productModel existingImages hasVariants optionsVariantsJson = do
  renderPageHeader productModel userMeta
  renderProductForm productModel existingImages hasVariants optionsVariantsJson

--------------------------------------------------------------------------------
-- Page header

renderPageHeader :: Products.Model -> UserMetadata.Model -> Lucid.Html ()
renderPageHeader productModel userMeta =
  Lucid.section_ [class_ $ base [Tokens.bgMain, Tokens.fgPrimary, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "EDIT PRODUCT"
        Lucid.div_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] $ do
          Lucid.strong_ "Product: "
          Lucid.toHtml productModel.pName
          " \x2022 "
          Lucid.strong_ "Editor: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.a_
        [ Lucid.href_ productsListUrl,
          hxGet_ productsListUrl,
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base [Tokens.infoText, Tokens.hoverBg, Tokens.textSm, "underline"]
        ]
        "VIEW PRODUCTS"

--------------------------------------------------------------------------------
-- Section 1: Basic Info Form

-- | Single form containing basic info, options/variants, and submit button.
--
-- The options/variants section is embedded via @plain@ as an Alpine.js
-- component with its own @x-data@ scope.  Its hidden input
-- (@variants_json@) is inside this form so it gets submitted.
renderProductForm :: Products.Model -> [ImageData] -> Bool -> Text -> Lucid.Html ()
renderProductForm productModel existingImages hasVariants optionsVariantsJson =
  renderForm config form
  where
    postUrl = productEditPostUrl productModel.pId

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = postUrl,
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcHtmxSwap = Just "innerHTML"
        }

    form :: FormBuilder
    form = do
      section "BASIC INFO" $ do
        textField "name" $ do
          label "Product Name"
          placeholder "e.g. KPBJ Tote Bag"
          value productModel.pName
          required
          maxLength 200

        textareaField "description" 6 $ do
          label "Description"
          placeholder "Describe the product..."
          value productModel.pDescription
          maxLength 5000

      section "PRICING & SHIPPING" $ do
        numberField "base_price_dollars" Nothing Nothing Nothing $ do
          label "Price ($)"
          placeholder "e.g. 24.99"
          value (Text.pack $ formatScientific Fixed (Just 2) (Cents.centsToDollars productModel.pBasePriceCents))
          hint "Enter the price in dollars (e.g. 24.99)"
          required

        numberField "weight_oz" Nothing Nothing Nothing $ do
          label "Weight (oz)"
          placeholder "e.g. 8"
          value (display productModel.pWeightOz)
          hint "Shipping weight in ounces"

      section "INVENTORY" $ do
        numberField "inventory_count" (Just 0) Nothing Nothing $ do
          label "Inventory Count"
          value (display productModel.pInventoryCount)
          when hasVariants disabled
          hint
            ( if hasVariants
                then "Inventory is tracked per variant. This field is ignored."
                else "Stock count for this product."
            )

      -- Product images (multi-image field with cropping and reorder)
      imagesField "product_images" $ do
        label "Product Images"
        maxSize 5
        aspectRatio (1, 1)
        previewSize 120
        currentImages existingImages
        hint "First image is the hero/thumbnail. Drag to reorder."

      -- Options & variants Alpine.js component (embedded in the form
      -- so the hidden variants_json input is submitted with the form)
      plain $
        renderOptionsAndVariants productModel optionsVariantsJson

      section "ORGANISATION" $ do
        textField "category" $ do
          label "Category"
          placeholder "e.g. apparel, vinyl, accessories"
          maybe (pure ()) value productModel.pCategory
          hint "Optional \x2014 used for filtering"
          maxLength 100

        numberField "sort_order" Nothing Nothing Nothing $ do
          label "Sort Order"
          value (display productModel.pSortOrder)
          hint "Lower numbers appear first (default 0)"

      footerToggle "is_active" $ do
        offLabel "Inactive"
        onLabel "Active"
        offValue ""
        onValue "on"
        when productModel.pIsActive checked
        hint "Toggle to make this product visible in the store"

      cancelButton productsListUrl "CANCEL"
      submitButton "UPDATE PRODUCT"

--------------------------------------------------------------------------------
-- Options & Variants (Alpine.js component)

-- | Combined Alpine.js component for managing product options and variants.
--
-- Replaces the old server-rendered option types (section 3) and variants (section 4)
-- with a single client-side component that:
--
-- 1. Lets staff add/remove option types and their values
-- 2. Auto-generates the cartesian product of variants
-- 3. Allows editing price, inventory, SKU, and weight per variant
-- 4. Serializes the full state as JSON in a hidden input for form submission
renderOptionsAndVariants :: Products.Model -> Text -> Lucid.Html ()
renderOptionsAndVariants productModel optionsVariantsJson =
  Lucid.section_
    [ Lucid.class_ "fb-section",
      xData_ (alpineState optionsVariantsJson productModel.pBasePriceCents productModel.pWeightOz)
    ]
    $ do
      Lucid.h2_ [Lucid.class_ "fb-section-title"] "OPTIONS & VARIANTS"

      -- Options area
      renderOptionsArea

      -- Add option button
      renderAddOptionButton

      -- Variants table
      renderVariantsTable

      -- Hidden input serializing the Alpine state as JSON for form submission
      Lucid.input_
        [ Lucid.type_ "hidden",
          Lucid.name_ "variants_json",
          xBindValue_ "serializeForSubmit()"
        ]

-- | Alpine.js state for the options/variants component.
alpineState :: Text -> Cents.Cents -> Int64 -> Text
alpineState optionsJson basePriceCents weightOz =
  [i|{
  ...#{optionsJson},
  productBasePrice: #{basePriceCents},
  productWeight: #{weightOz},
  newValueInputs: {},

  addOption() {
    this.options.push({name: '', values: []});
  },
  removeOption(idx) {
    const removed = this.options.splice(idx, 1)[0];
    this.variants.forEach(v => {
      if (v.id) this.deletedVariantIds.push(v.id);
    });
    this.regenerateVariants();
  },
  addValue(optIdx, value) {
    if (!value.trim()) return;
    this.options[optIdx].values.push(value.trim());
    this.newValueInputs[optIdx] = '';
    this.regenerateVariants();
  },
  removeValue(optIdx, valIdx) {
    const removed = this.options[optIdx].values.splice(valIdx, 1)[0];
    this.variants.filter(v => v.option_values.includes(removed)).forEach(v => {
      if (v.id) this.deletedVariantIds.push(v.id);
    });
    this.regenerateVariants();
  },
  regenerateVariants() {
    if (this.options.length === 0 || this.options.every(o => o.values.length === 0)) {
      this.variants = [];
      return;
    }
    const combos = this.cartesian(this.options.map(o => o.values));
    const oldVariants = [...this.variants];
    this.variants = combos.map(combo => {
      const existing = oldVariants.find(v =>
        JSON.stringify(v.option_values) === JSON.stringify(combo)
      );
      if (existing) return {...existing, option_values: combo};
      return {id: null, option_values: combo, price_cents: null, inventory_count: 0, sku: null, weight_oz: null};
    });
  },
  cartesian(arrays) {
    if (arrays.length === 0) return [];
    return arrays.reduce((acc, arr) =>
      acc.flatMap(combo => arr.map(val => [...combo, val])),
      [[]]
    );
  },
  deleteVariant(idx) {
    const v = this.variants[idx];
    if (v.id) this.deletedVariantIds.push(v.id);
    this.variants.splice(idx, 1);
  },
  formatPrice(cents) {
    if (cents === null || cents === undefined) return '';
    return (cents / 100).toFixed(2);
  },
  parsePriceCents(dollarStr) {
    if (!dollarStr || dollarStr.trim() === '') return null;
    const val = parseFloat(dollarStr);
    if (isNaN(val)) return null;
    return Math.round(val * 100);
  },
  serializeForSubmit() {
    const cleanVariants = this.variants.map(v => ({
      id: v.id,
      option_values: v.option_values,
      price_cents: v.price_cents === '' || v.price_cents === null ? null : Number(v.price_cents),
      inventory_count: Number(v.inventory_count) || 0,
      sku: v.sku === '' ? null : v.sku,
      weight_oz: v.weight_oz === '' || v.weight_oz === null ? null : Number(v.weight_oz)
    }));
    return JSON.stringify({
      options: this.options,
      variants: cleanVariants,
      deleted_variant_ids: this.deletedVariantIds
    });
  }
}|]

-- | Render the options area with dynamic x-for loops.
renderOptionsArea :: Lucid.Html ()
renderOptionsArea =
  Lucid.div_ [class_ $ base ["space-y-4", Tokens.mb4]] $
    Lucid.template_
      [xFor_ "(opt, optIdx) in options", xKey_ "optIdx"]
      renderOptionRow

-- | Render a single option row with name input, value tags, and add value input.
renderOptionRow :: Lucid.Html ()
renderOptionRow =
  Lucid.div_ [class_ $ base [Tokens.border2, Tokens.p4]] $ do
    -- Option name + remove button
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", Tokens.mb2]] $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.placeholder_ "Option name (e.g. Size, Color)",
          xModel_ "opt.name",
          class_ $ base ["flex-1", Tokens.bgMain, Tokens.fgPrimary, Tokens.border2, Tokens.p2, Tokens.textSm, Tokens.fontBold]
        ]
      Lucid.button_
        [ Lucid.type_ "button",
          xOnClick_ "removeOption(optIdx)",
          class_ $ base [Tokens.errorText, Tokens.hoverBg, Tokens.textSm, Tokens.fontBold, "px-2", "py-1", "ml-2"]
        ]
        "REMOVE"

    -- Existing values as tags
    Lucid.div_ [class_ $ base ["flex", "flex-wrap", "gap-2", "mb-3"]]
      $ Lucid.template_
        [xFor_ "(val, valIdx) in opt.values", xKey_ "valIdx"]
      $ Lucid.span_
        [class_ $ base ["inline-flex", "items-center", "gap-1", Tokens.border2, "px-2", "py-1", Tokens.textSm]]
      $ do
        Lucid.span_ [xText_ "val"] mempty
        Lucid.button_
          [ Lucid.type_ "button",
            xOnClick_ "removeValue(optIdx, valIdx)",
            class_ $ base [Tokens.errorText, "ml-1", Tokens.textSm]
          ]
          "\xd7"

    -- Add value input
    Lucid.div_ [class_ $ base ["flex", "gap-2", "items-end"]] $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.placeholder_ "Add value...",
          xModel_ "newValueInputs[optIdx]",
          xOn_ "keydown.enter.prevent" "addValue(optIdx, newValueInputs[optIdx] || '')",
          class_ $ base [Tokens.bgMain, Tokens.fgPrimary, Tokens.border2, Tokens.p2, Tokens.textSm, "flex-1"]
        ]
      Lucid.button_
        [ Lucid.type_ "button",
          xOnClick_ "addValue(optIdx, newValueInputs[optIdx] || '')",
          class_ $ base [Tokens.bgAlt, Tokens.fgPrimary, Tokens.px3, Tokens.py2, Tokens.textSm, Tokens.fontBold, Tokens.hoverBg]
        ]
        "ADD VALUE"

-- | Render the add option button.
renderAddOptionButton :: Lucid.Html ()
renderAddOptionButton =
  Lucid.div_ [class_ $ base [Tokens.mb4]] $
    Lucid.button_
      [ Lucid.type_ "button",
        xOnClick_ "addOption()",
        class_ $ base [Tokens.bgAlt, Tokens.fgPrimary, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, Tokens.hoverBg, "w-full"]
      ]
      "+ ADD OPTION TYPE"

-- | Render the auto-generated variants table.
renderVariantsTable :: Lucid.Html ()
renderVariantsTable =
  Lucid.div_ [xShow_ "variants.length > 0"] $ do
    Lucid.h3_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.fgMuted, Tokens.mb2]] "GENERATED VARIANTS"
    Lucid.table_ [class_ $ base ["w-full", Tokens.textSm, Tokens.border2]] $ do
      Lucid.thead_ $
        Lucid.tr_ [class_ $ base [Tokens.bgAlt]] $ do
          Lucid.th_ [class_ $ base [Tokens.p2, "text-left", Tokens.fontBold]] "Variant"
          Lucid.th_ [class_ $ base [Tokens.p2, "text-left", Tokens.fontBold]] "Price ($)"
          Lucid.th_ [class_ $ base [Tokens.p2, "text-left", Tokens.fontBold]] "Inventory"
          Lucid.th_ [class_ $ base [Tokens.p2, "text-left", Tokens.fontBold]] "SKU"
          Lucid.th_ [class_ $ base [Tokens.p2, "text-left", Tokens.fontBold]] "Weight (oz)"
          Lucid.th_ [class_ $ base [Tokens.p2, "text-left", Tokens.fontBold]] ""
      Lucid.tbody_ $
        Lucid.template_
          [xFor_ "(variant, vIdx) in variants", xKey_ "vIdx"]
          renderVariantRow

-- | Render a single variant row in the table.
renderVariantRow :: Lucid.Html ()
renderVariantRow =
  Lucid.tr_ [class_ $ base [Tokens.border2]] $ do
    -- Variant label (computed from option_values)
    Lucid.td_ [class_ $ base [Tokens.p2, Tokens.fontBold]] $
      Lucid.span_ [xText_ "variant.option_values.join(' / ')"] mempty

    -- Price ($) input
    Lucid.td_ [class_ $ base [Tokens.p2]] $
      Lucid.input_
        [ Lucid.type_ "number",
          makeAttributes "step" "0.01",
          makeAttributes "min" "0",
          Lucid.placeholder_ "Base price",
          xBindValue_ "formatPrice(variant.price_cents)",
          xOnChange_ "variant.price_cents = parsePriceCents($event.target.value)",
          class_ $ base ["w-24", Tokens.bgMain, Tokens.fgPrimary, Tokens.border2, "p-1", Tokens.textSm]
        ]

    -- Inventory input
    Lucid.td_ [class_ $ base [Tokens.p2]] $
      Lucid.input_
        [ Lucid.type_ "number",
          makeAttributes "min" "0",
          xModel_ "variant.inventory_count",
          class_ $ base ["w-20", Tokens.bgMain, Tokens.fgPrimary, Tokens.border2, "p-1", Tokens.textSm]
        ]

    -- SKU input
    Lucid.td_ [class_ $ base [Tokens.p2]] $
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.placeholder_ "SKU",
          xModel_ "variant.sku",
          class_ $ base ["w-24", Tokens.bgMain, Tokens.fgPrimary, Tokens.border2, "p-1", Tokens.textSm]
        ]

    -- Weight (oz) input
    Lucid.td_ [class_ $ base [Tokens.p2]] $
      Lucid.input_
        [ Lucid.type_ "number",
          makeAttributes "min" "0",
          Lucid.placeholder_ "Base",
          xBindValue_ "variant.weight_oz === null ? '' : variant.weight_oz",
          xOnChange_ "variant.weight_oz = $event.target.value === '' ? null : parseInt($event.target.value, 10)",
          class_ $ base ["w-20", Tokens.bgMain, Tokens.fgPrimary, Tokens.border2, "p-1", Tokens.textSm]
        ]

    -- Delete button
    Lucid.td_ [class_ $ base [Tokens.p2]] $
      Lucid.button_
        [ Lucid.type_ "button",
          xOnClick_ "deleteVariant(vIdx)",
          class_ $ base [Tokens.errorText, Tokens.textSm, Tokens.fontBold, "px-2", "py-1"]
        ]
        "\xd7"
