{-# LANGUAGE QuasiQuotes #-}

module API.Store.Cart.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (storeApiLinks, storeLinks)
import API.Types (StoreApiRoutes (..), StoreRoutes (..))
import Component.PageHeader (pageHeader)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Alpine
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Cart page template.
--
-- Renders a shell that Alpine.js populates client-side. On load, if the
-- cart has items, fires a POST to the validate endpoint to get the
-- server-rendered cart fragment.
template :: Lucid.Html ()
template = do
  pageHeader "CART"

  Lucid.div_
    [ xData_ cartAlpineState,
      xInit_ "validateCart()",
      class_ $ base ["max-w-lg", "mx-auto"]
    ]
    $ do
      -- Empty state (shown when cart is empty)
      Lucid.div_ [xShow_ "isEmpty", class_ $ base ["text-center", Tokens.py8]] $ do
        Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb4]] "Your cart is empty."
        Lucid.a_
          [ Lucid.href_ [i|/#{storeListUrl}|],
            hxGet_ [i|/#{storeListUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [Tokens.linkText]
          ]
          "Continue Shopping"

      -- Cart items container (populated by validate endpoint)
      Lucid.div_ [xShow_ "!isEmpty"] $ do
        Lucid.div_ [xShow_ "loading", class_ $ base ["text-center", Tokens.py8]] $
          Lucid.p_ [class_ $ base [Tokens.fgMuted]] "Loading cart..."
        Lucid.div_ [Lucid.id_ "cart-items-container", xShow_ "!loading"] mempty

--------------------------------------------------------------------------------
-- URL helpers

storeListUrl :: Links.URI
storeListUrl = Links.linkURI storeLinks.list

validateUrl :: Links.URI
validateUrl = Links.linkURI storeApiLinks.cartValidate

--------------------------------------------------------------------------------
-- Alpine.js state

-- | Alpine.js state for the cart page.
--
-- Reads items from the global @Alpine.store('cart')@ and fires a POST
-- to the validate endpoint to get the server-rendered cart fragment.
cartAlpineState :: Text
cartAlpineState =
  [i|{
  loading: false,
  get cartItems() { return Alpine.store('cart').items; },
  get isEmpty() { return this.cartItems.length === 0; },
  validateCart() {
    if (this.isEmpty) return;
    this.loading = true;
    fetch('/#{validateUrl}', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(this.cartItems)
    })
    .then(r => r.text())
    .then(html => {
      document.getElementById('cart-items-container').innerHTML = html;
      const dataEl = document.getElementById('validated-cart-data');
      if (dataEl) {
        try {
          Alpine.store('cart').items = JSON.parse(dataEl.dataset.items);
        } catch(e) {}
      }
      this.loading = false;
    })
    .catch(() => { this.loading = false; });
  }
}|]
