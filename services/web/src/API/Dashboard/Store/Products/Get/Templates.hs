{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Store.Products.Get.Templates (template, renderProductRow) where

--------------------------------------------------------------------------------

import API.Links (dashboardStoreProductsLinks, rootLink)
import API.Types (DashboardStoreProductsRoutes (..))
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    clickableCellAttrs,
    renderIndexTable,
    rowAttrs,
  )
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Domain.Types.Cents qualified as Cents
import Effects.Database.Tables.Products qualified as Products
import Lucid qualified
import Lucid.Alpine (xData_, xOnClick_, xOn_, xShow_)
import Lucid.HTMX (hxInclude_, hxOnAfterRequest_, hxPost_, hxSwap_, hxTarget_)

--------------------------------------------------------------------------------

-- | Products list template.
template :: [Products.Model] -> Lucid.Html ()
template products =
  Lucid.div_
    [ xData_ "{ showCreate: false }",
      xOn_ "show-create.window" "showCreate = true",
      xOn_ "hide-create.window" "showCreate = false"
    ]
    $ Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]]
    $ renderIndexTable
      IndexTableConfig
        { itcBodyId = "products-table-body",
          itcHeaders =
            [ ColumnHeader "Name" AlignLeft,
              ColumnHeader "Category" AlignLeft,
              ColumnHeader "Price" AlignLeft,
              ColumnHeader "Inventory" AlignLeft,
              ColumnHeader "Status" AlignLeft
            ],
          itcNextPageUrl = Nothing,
          itcPaginationConfig = Nothing
        }
      ( do
          if null products
            then renderEmptyRow
            else mapM_ renderProductRow products
          renderInlineCreateRow
      )

renderProductRow :: Products.Model -> Lucid.Html ()
renderProductRow productModel =
  let productId = productModel.pId
      productIdText = display productId
      rowId = [i|product-row-#{productIdText}|] :: Text
      editUrl = rootLink $ dashboardStoreProductsLinks.editGet productId
   in Lucid.tr_ (rowAttrs rowId) $ do
        Lucid.td_ (clickableCellAttrs editUrl) $
          Lucid.span_ [Lucid.class_ Tokens.fontBold] $
            Lucid.toHtml productModel.pName

        Lucid.td_ (clickableCellAttrs editUrl) $
          Lucid.div_ [Lucid.class_ Tokens.textSm] $
            maybe (Lucid.toHtml ("\x2014" :: Text)) Lucid.toHtml productModel.pCategory

        Lucid.td_ (clickableCellAttrs editUrl) $
          Lucid.div_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml (Cents.formatDisplay productModel.pBasePriceCents)

        Lucid.td_ (clickableCellAttrs editUrl) $
          Lucid.div_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml (display productModel.pInventoryCount)

        Lucid.td_ (clickableCellAttrs editUrl) $
          renderStatusBadge productModel.pIsActive

-- | Inline create row shown when showCreate Alpine state is true.
--
-- The tr itself carries the hx-post/hx-target/hx-swap attributes.
-- HTMX collects all named inputs within the element when a submit button is clicked.
renderInlineCreateRow :: Lucid.Html ()
renderInlineCreateRow =
  Lucid.tr_
    [ Lucid.id_ "inline-create-row",
      xShow_ "showCreate",
      class_ $ base ["border-b", Theme.borderMuted]
    ]
    $ do
      -- Name
      Lucid.td_ [class_ $ base [Tokens.p2]] $
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.name_ "name",
            Lucid.placeholder_ "Product name",
            Lucid.required_ "true",
            class_ $ base [Tokens.bgMain, Tokens.fgPrimary, Tokens.border2, Tokens.p2, Tokens.textSm, "w-full"]
          ]
      -- Category
      Lucid.td_ [class_ $ base [Tokens.p2]] $
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.name_ "category",
            Lucid.placeholder_ "Category",
            class_ $ base [Tokens.bgMain, Tokens.fgPrimary, Tokens.border2, Tokens.p2, Tokens.textSm, "w-full"]
          ]
      -- Price
      Lucid.td_ [class_ $ base [Tokens.p2]] $
        Lucid.input_
          [ Lucid.type_ "number",
            Lucid.name_ "base_price_dollars",
            Lucid.step_ "0.01",
            Lucid.min_ "0",
            Lucid.placeholder_ "0.00",
            class_ $ base [Tokens.bgMain, Tokens.fgPrimary, Tokens.border2, Tokens.p2, Tokens.textSm, "w-full"]
          ]
      -- Inventory
      Lucid.td_ [class_ $ base [Tokens.p2]] $
        Lucid.input_
          [ Lucid.type_ "number",
            Lucid.name_ "inventory_count",
            Lucid.value_ "0",
            Lucid.min_ "0",
            class_ $ base [Tokens.bgMain, Tokens.fgPrimary, Tokens.border2, Tokens.p2, Tokens.textSm, "w-full"]
          ]
      -- Actions (in the Status column position)
      Lucid.td_ [class_ $ base [Tokens.p2]] $
        Lucid.div_ [class_ $ base ["flex", "gap-2", "items-center"]] $ do
          Lucid.button_
            [ Lucid.type_ "button",
              hxPost_ (rootLink dashboardStoreProductsLinks.inlineCreate),
              hxTarget_ "#inline-create-row",
              hxSwap_ "beforebegin",
              hxInclude_ "closest tr",
              hxOnAfterRequest_ "if(event.detail.successful){ window.dispatchEvent(new Event('hide-create')); this.closest('tr').querySelectorAll('input').forEach(i => i.value = i.defaultValue) }",
              class_ $ base [Tokens.bgAlt, Tokens.fgPrimary, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, Tokens.hoverBg]
            ]
            "SAVE"
          Lucid.button_
            [ Lucid.type_ "button",
              xOnClick_ "showCreate = false",
              class_ $ base [Theme.fgMuted, Tokens.hoverBg, Tokens.textSm, "px-2", "py-1"]
            ]
            "\xd7"

-- | Empty row shown when no products exist.
renderEmptyRow :: Lucid.Html ()
renderEmptyRow =
  Lucid.tr_ [xShow_ "!showCreate"] $
    Lucid.td_
      [ Lucid.colspan_ "5",
        class_ $ base [Theme.fgMuted, "text-center", "py-12"]
      ]
      $ do
        Lucid.p_ [class_ $ base [Tokens.textXl]] "No products yet."
        Lucid.p_ [class_ $ base ["mt-2"]] "Click + NEW above to create your first product."

renderStatusBadge :: Bool -> Lucid.Html ()
renderStatusBadge isActive =
  let (bgClass, textClass, statusText) =
        if isActive
          then (Tokens.successBg, Tokens.successText, "Active") :: (Text, Text, Text)
          else (Tokens.warningBg, Tokens.warningText, "Inactive")
   in Lucid.span_
        [class_ $ base ["inline-block", "px-3", "py-1", Tokens.textSm, Tokens.fontBold, "rounded", bgClass, textClass]]
        $ Lucid.toHtml statusText
