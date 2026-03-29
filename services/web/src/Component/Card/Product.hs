{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Component.Card.Product
  ( renderProductCard,
  )
where

--------------------------------------------------------------------------------

import API.Links (storeLinks)
import API.Types
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cents qualified as Cents
import Domain.Types.Slug (Slug (..))
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.Products qualified as Products
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

productGetUrl :: Text -> Links.URI
productGetUrl slug = Links.linkURI $ storeLinks.product (Slug slug)

--------------------------------------------------------------------------------

-- | Render a product card for the store listing grid.
renderProductCard ::
  StorageBackend ->
  -- | The product
  Products.Model ->
  -- | Hero image path
  Maybe Text ->
  -- | Hero image alt text
  Maybe Text ->
  Lucid.Html ()
renderProductCard backend prod mImagePath mAltText = do
  let slug = prod.pSlug
      name = prod.pName
      price = prod.pBasePriceCents
      inventory = prod.pInventoryCount
  Lucid.a_
    [ Lucid.href_ [i|/#{productGetUrl slug}|],
      hxGet_ [i|/#{productGetUrl slug}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "block"
    ]
    $ do
      -- Product Image (square 1:1) or placeholder
      Lucid.div_ [class_ $ base [Tokens.fullWidth, "aspect-square", "overflow-hidden", "border", Tokens.borderMuted]] $ do
        case mImagePath of
          Just imagePath ->
            Lucid.img_
              [ Lucid.src_ (buildMediaUrl backend imagePath),
                Lucid.alt_ (fromMaybe (name <> " product image") mAltText),
                Lucid.class_ "w-full h-full object-cover"
              ]
          Nothing ->
            Lucid.div_ [class_ $ base [Tokens.fullWidth, "h-full", Tokens.bgAlt]] mempty

      -- Product Name
      Lucid.h3_ [class_ $ base [Tokens.fontBold, Tokens.mb2, "mt-2"]] $
        Lucid.toHtml name

      -- Price
      Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $
        Lucid.toHtml (Cents.formatDisplay price)

      -- Out of Stock label
      when (inventory <= 0) $
        Lucid.p_ [class_ $ base [Tokens.textSm, "text-red-600", Tokens.fontBold, "mt-1"]] "OUT OF STOCK"
