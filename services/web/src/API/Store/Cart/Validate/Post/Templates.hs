{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Store.Cart.Validate.Post.Templates
  ( -- * Types
    ValidatedCartItem (..),
    CartWarning (..),

    -- * Template
    template,
  )
where

--------------------------------------------------------------------------------

import API.Links (storeLinks)
import API.Types (StoreRoutes (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_, traverse_)
import Data.List (foldl')
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cents (Cents)
import Domain.Types.Cents qualified as Cents
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Lucid qualified
import Lucid.Alpine
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | A cart item that has been validated against current DB state.
data ValidatedCartItem = ValidatedCartItem
  { vciProductId :: !Products.Id,
    vciVariantId :: !(Maybe ProductVariants.Id),
    vciProductName :: !Text,
    vciVariantLabel :: !(Maybe Text),
    vciUnitPriceCents :: !Cents,
    vciQuantity :: !Int,
    vciHeroImagePath :: !(Maybe Text)
  }
  deriving stock (Show)

-- | A warning generated during cart validation.
data CartWarning = CartWarning
  { cwProductName :: !Text,
    cwMessage :: !Text
  }
  deriving stock (Show)

--------------------------------------------------------------------------------
-- URL helpers

storeListUrl :: Links.URI
storeListUrl = Links.linkURI storeLinks.list

checkoutUrl :: Links.URI
checkoutUrl = Links.linkURI storeLinks.checkout

--------------------------------------------------------------------------------

-- | Render the validated cart fragment.
--
-- This is a raw HTML fragment (NOT wrapped in renderTemplate) that gets
-- swapped into the cart page's @#cart-items-container@ by the client-side
-- fetch call.
template ::
  StorageBackend ->
  [ValidatedCartItem] ->
  [CartWarning] ->
  Lucid.Html ()
template backend items warnings = do
  -- Warnings
  traverse_ renderWarning warnings

  -- Cart items
  Lucid.div_ [class_ $ base ["space-y-4", Tokens.mb6]] $
    traverse_ (renderCartItem backend) items

  -- Subtotal
  renderSubtotal items

  -- Shipping note
  Lucid.p_
    [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb4, "text-center"]]
    "Shipping calculated at checkout."

  -- Checkout button
  Lucid.a_
    [ Lucid.href_ [i|/#{checkoutUrl}|],
      hxGet_ [i|/#{checkoutUrl}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      class_ $ base [Tokens.fullWidth, Tokens.buttonPrimary, "text-center", "block", Tokens.mb4]
    ]
    "CHECKOUT"

  -- Continue shopping link
  Lucid.div_ [class_ $ base ["text-center"]] $
    Lucid.a_
      [ Lucid.href_ [i|/#{storeListUrl}|],
        hxGet_ [i|/#{storeListUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base [Tokens.linkText]
      ]
      "Continue Shopping"

  -- Hidden element with validated cart items as JSON.
  -- The cart page reads this after innerHTML swap to sync localStorage,
  -- removing items the server rejected (deleted, inactive, out of stock).
  Lucid.div_
    [ Lucid.id_ "validated-cart-data",
      Lucid.style_ "display:none",
      Lucid.data_ "items" (validCartJson items)
    ]
    mempty

--------------------------------------------------------------------------------
-- Warning

renderWarning :: CartWarning -> Lucid.Html ()
renderWarning warning =
  Lucid.div_
    [ class_ $ base ["border", Tokens.warningBorder, Tokens.warningBg, Tokens.p4, Tokens.mb4, Tokens.textSm]
    ]
    $ do
      Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.warningText]] $
        Lucid.toHtml warning.cwProductName
      Lucid.span_ [class_ $ base [Tokens.warningText]] $ do
        ": "
        Lucid.toHtml warning.cwMessage

--------------------------------------------------------------------------------
-- Cart Item

renderCartItem :: StorageBackend -> ValidatedCartItem -> Lucid.Html ()
renderCartItem backend item = do
  let productIdRaw = show (Products.unId item.vciProductId) :: String
      variantIdJs = maybe ("null" :: String) (show . ProductVariants.unId) item.vciVariantId
      qty = item.vciQuantity
      qtyDec = show (qty - 1) :: String
      qtyInc = show (qty + 1) :: String
      lineTotal = Cents.Cents (fromIntegral qty * Cents.unCents item.vciUnitPriceCents)

  Lucid.div_
    [ class_ $ base ["flex", Tokens.gap4, "items-start", "border-b", Tokens.borderMuted, Tokens.py4]
    ]
    $ do
      -- Thumbnail
      renderThumbnail backend item.vciHeroImagePath item.vciProductName

      -- Item details
      Lucid.div_ [class_ $ base ["flex-1", "min-w-0"]] $ do
        -- Product name
        Lucid.p_ [class_ $ base [Tokens.fontBold]] $
          Lucid.toHtml item.vciProductName

        -- Variant label
        for_ item.vciVariantLabel $ \label' ->
          Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $
            Lucid.toHtml label'

        -- Unit price
        Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $
          Lucid.toHtml (Cents.formatDisplay item.vciUnitPriceCents)

        -- Quantity stepper
        Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap2, Tokens.mt4]] $ do
          -- Decrement
          Lucid.button_
            [ Lucid.type_ "button",
              class_ $ base [Tokens.px3, Tokens.py2, "border", Tokens.borderMuted, Tokens.fontBold, "select-none", "cursor-pointer"],
              xOnClick_ [i|Alpine.store('cart').updateQuantity(#{productIdRaw}, #{variantIdJs}, #{qtyDec}); validateCart()|]
            ]
            "\x2212"

          -- Current quantity
          Lucid.span_ [class_ $ base [Tokens.px3, "min-w-[2rem]", "text-center"]] $
            Lucid.toHtml (show qty)

          -- Increment
          Lucid.button_
            [ Lucid.type_ "button",
              class_ $ base [Tokens.px3, Tokens.py2, "border", Tokens.borderMuted, Tokens.fontBold, "select-none", "cursor-pointer"],
              xOnClick_ [i|Alpine.store('cart').updateQuantity(#{productIdRaw}, #{variantIdJs}, #{qtyInc}); validateCart()|]
            ]
            "+"

          -- Remove
          Lucid.button_
            [ Lucid.type_ "button",
              class_ $ base [Tokens.textSm, Tokens.fgMuted, "underline", "cursor-pointer", "ml-auto"],
              xOnClick_ [i|Alpine.store('cart').removeItem(#{productIdRaw}, #{variantIdJs}); validateCart()|]
            ]
            "Remove"

      -- Line total
      Lucid.div_ [class_ $ base [Tokens.fontBold, "text-right", "flex-shrink-0"]] $
        Lucid.toHtml (Cents.formatDisplay lineTotal)

--------------------------------------------------------------------------------
-- Thumbnail

renderThumbnail :: StorageBackend -> Maybe Text -> Text -> Lucid.Html ()
renderThumbnail backend mImagePath altText =
  Lucid.div_ [class_ $ base ["w-16", "h-16", "flex-shrink-0", "border", Tokens.borderMuted, "overflow-hidden"]] $
    case mImagePath of
      Just path' ->
        Lucid.img_
          [ Lucid.src_ (buildMediaUrl backend path'),
            Lucid.alt_ altText,
            Lucid.class_ "w-full h-full object-cover"
          ]
      Nothing ->
        Lucid.div_
          [class_ $ base ["w-full", "h-full", Tokens.bgAlt, "flex", "items-center", "justify-center"]]
          $ Lucid.span_ [class_ $ base [Tokens.fgMuted, Tokens.textXs]] "N/A"

--------------------------------------------------------------------------------
-- Subtotal

renderSubtotal :: [ValidatedCartItem] -> Lucid.Html ()
renderSubtotal items = do
  let subtotal = foldl' addItemTotal (Cents.Cents 0) items
  Lucid.div_
    [ class_ $ base ["flex", "justify-between", "items-center", Tokens.py4, Tokens.mb4]
    ]
    $ do
      Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textLg]] "Subtotal"
      Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textLg]] $
        Lucid.toHtml (Cents.formatDisplay subtotal)
  where
    addItemTotal :: Cents -> ValidatedCartItem -> Cents
    addItemTotal acc item =
      acc + Cents.Cents (fromIntegral item.vciQuantity * Cents.unCents item.vciUnitPriceCents)

-- | Serialize validated cart items to JSON for client-side cart sync.
--
-- Produces the same shape as the client-side cart: @[{productId, variantId, quantity}]@.
validCartJson :: [ValidatedCartItem] -> Text
validCartJson items =
  Text.decodeUtf8 . BSL.toStrict . Aeson.encode $
    map
      ( \item ->
          Aeson.object
            [ "productId" Aeson..= Products.unId item.vciProductId,
              "variantId" Aeson..= fmap ProductVariants.unId item.vciVariantId,
              "quantity" Aeson..= item.vciQuantity
            ]
      )
      items
