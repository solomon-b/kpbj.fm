module API.Store.List.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import Component.Card.Product (renderProductCard)
import Component.PageHeader (pageHeader)
import Data.Foldable (traverse_)
import Design (base, class_, desktop, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.Products qualified as Products
import Lucid qualified

--------------------------------------------------------------------------------

-- | Store listing page template.
--
-- Shows a responsive grid of product cards, or an empty state message
-- when no products are available.
template :: StorageBackend -> [Products.ProductWithHeroImage] -> Lucid.Html ()
template backend products = do
  pageHeader "STORE"

  Lucid.section_ [Lucid.id_ "store-content-container", Lucid.class_ "w-full"] $ do
    Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.textSm, Tokens.mb6, "text-center"]]
      "Support KPBJ \x2014 all proceeds go directly to the station."

    if null products
      then Lucid.div_ [class_ $ base [Tokens.cardBase, "text-center"]] $ do
        Lucid.h2_ [class_ $ base [Tokens.headingLg, Tokens.mb4]] "No Products Available"
        Lucid.p_ [Lucid.class_ Tokens.fgMuted] "Check back soon!"
      else
        Lucid.div_
          [ class_ $ do
              base ["grid", "grid-cols-1", Tokens.gap6, "max-w-4xl", "mx-auto"]
              tablet ["grid-cols-2"]
              desktop ["grid-cols-3"]
          ]
          $ traverse_ renderCard products
  where
    renderCard pwh =
      renderProductCard
        backend
        (Products.pwhToProduct pwh)
        (Products.pwhHeroImagePath pwh)
        (Products.pwhHeroAltText pwh)
