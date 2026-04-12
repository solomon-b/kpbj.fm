{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Store.Orders.Confirmation.Get.Templates
  ( template,
    notFoundTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Links (rootLink, storeLinks)
import API.Types (StoreRoutes (..))
import Component.PageHeader (pageHeader)
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.Orders qualified as Orders
import Lucid qualified
import Store.Checkout.Logic (maskEmail)

--------------------------------------------------------------------------------

-- | Order confirmation page template.
--
-- Shown after Stripe redirects the customer back. Lets the customer know
-- their order is being processed and that a confirmation email is on the way.
template :: Orders.Model -> Lucid.Html ()
template order = do
  pageHeader "ORDER CONFIRMED"

  Lucid.div_ [class_ $ base ["max-w-lg", "mx-auto"]] $ do
    Lucid.div_ [class_ $ base [Tokens.cardBase, "text-center"]] $ do
      Lucid.h2_
        [class_ $ base [Tokens.headingLg, Tokens.mb4]]
        [i|Thank you! Order #{orderNumber} is being processed.|]

      Lucid.p_
        [class_ $ base [Tokens.fgMuted, Tokens.mb6]]
        [i|You'll receive a confirmation email at #{maskedEmail} shortly.|]

      Lucid.a_
        [ Lucid.href_ (rootLink storeLinks.list),
          class_ $ base [Tokens.linkText]
        ]
        "Back to Store"

    -- Clear the cart after a successful purchase
    Lucid.script_ "if (window.Alpine) { Alpine.store('cart').items = []; } localStorage.removeItem('kpbj-cart');"
  where
    orderNumber = order.oOrderNumber
    maskedEmail = maskEmail order.oEmail

-- | Rendered when no order matches the given order number.
notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  pageHeader "ORDER NOT FOUND"

  Lucid.div_ [class_ $ base ["max-w-lg", "mx-auto"]] $ do
    Lucid.div_ [class_ $ base [Tokens.cardBase, "text-center"]] $ do
      Lucid.h2_ [class_ $ base [Tokens.headingLg, Tokens.mb4]] "Order Not Found"

      Lucid.p_
        [class_ $ base [Tokens.fgMuted, Tokens.mb6]]
        "We couldn't find that order. Please check your order number and try again."

      Lucid.a_
        [ Lucid.href_ (rootLink storeLinks.list),
          class_ $ base [Tokens.linkText]
        ]
        "Back to Store"
