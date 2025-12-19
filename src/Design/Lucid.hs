-- | Lucid HTML components for KPBJ templates.
--
-- This module re-exports all Lucid component modules for convenience.
--
-- > import Design.Lucid (container, card, desktopOnly)
--
-- For more targeted imports:
--
-- > import Design.Lucid.Layout (container, grid)
-- > import Design.Lucid.Card (card, cardWithHeader)
-- > import Design.Lucid.Visibility (desktopOnly, mobileOnly)
module Design.Lucid
  ( -- * Layout Components
    module Design.Lucid.Layout,

    -- * Card Components
    module Design.Lucid.Card,

    -- * Visibility Components
    module Design.Lucid.Visibility,
  )
where

--------------------------------------------------------------------------------

import Design.Lucid.Card
import Design.Lucid.Layout
import Design.Lucid.Visibility
