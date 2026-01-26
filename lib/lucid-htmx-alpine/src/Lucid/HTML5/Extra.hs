-- | Extra HTML5 and SVG elements/attributes for Lucid2.
--
-- This module provides elements and attributes that are missing from
-- the standard Lucid2 library.
module Lucid.HTML5.Extra
  ( -- * SVG Elements
    svg_,
    path_,

    -- * SVG Attributes
    viewBox_,
    d_,

    -- * ARIA Attributes
    ariaHidden_,
    ariaLabelledby_,

    -- * Generic Data Attributes
    dataViewComponent_,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Lucid qualified
import Lucid.Base qualified

--------------------------------------------------------------------------------
-- SVG Elements

-- | SVG container element.
--
-- > svg_ [viewBox_ "0 0 24 24"] $ do
-- >   path_ [d_ "M12 ..."] mempty
svg_ :: [Lucid.Base.Attributes] -> Lucid.Html () -> Lucid.Html ()
svg_ = Lucid.Base.term "svg"

-- | SVG path element.
--
-- > path_ [d_ "M12 2L2 22h20L12 2z"] mempty
path_ :: [Lucid.Base.Attributes] -> Lucid.Html () -> Lucid.Html ()
path_ = Lucid.Base.term "path"

--------------------------------------------------------------------------------
-- SVG Attributes

-- | SVG viewBox attribute.
--
-- > viewBox_ "0 0 24 24"
viewBox_ :: Text -> Lucid.Base.Attributes
viewBox_ = Lucid.Base.makeAttributes "viewBox"

-- | SVG d (path data) attribute.
--
-- > d_ "M12 2L2 22h20L12 2z"
d_ :: Text -> Lucid.Base.Attributes
d_ = Lucid.Base.makeAttributes "d"

--------------------------------------------------------------------------------
-- ARIA Attributes

-- | ARIA aria-hidden attribute.
--
-- > ariaHidden_ "true"
ariaHidden_ :: Text -> Lucid.Base.Attributes
ariaHidden_ = Lucid.Base.makeAttributes "aria-hidden"

-- | ARIA aria-labelledby attribute.
--
-- > ariaLabelledby_ "title-id"
ariaLabelledby_ :: Text -> Lucid.Base.Attributes
ariaLabelledby_ = Lucid.Base.makeAttributes "aria-labelledby"

--------------------------------------------------------------------------------
-- Data Attributes

-- | Generic data-view-component attribute (used by GitHub Primer).
--
-- > dataViewComponent_ "true"
dataViewComponent_ :: Text -> Lucid.Base.Attributes
dataViewComponent_ = Lucid.Base.makeAttributes "data-view-component"
