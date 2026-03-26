module API.Dashboard.Store.Orders.Get.Templates (template) where

--------------------------------------------------------------------------------

import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified

--------------------------------------------------------------------------------

-- | Orders placeholder template.
--
-- Order management is not yet implemented.
template :: Lucid.Html ()
template =
  Lucid.section_ [class_ $ base [Tokens.bgMain, Tokens.p6]] $ do
    Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4]] "ORDERS"
    Lucid.p_ [class_ $ base [Tokens.fgMuted]] "Order management coming soon."
