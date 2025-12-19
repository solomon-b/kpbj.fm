-- | DEPRECATED: Use "Design.Lucid" or "Design.Lucid.Layout" instead.
--
-- This module is kept for backwards compatibility during migration.
-- It re-exports from the new Design.Lucid modules.
module Component.Layout
  ( -- * Container Components
    container,
    narrowContainer,
    wideContainer,

    -- * Section Components
    section,
    heroSection,
    cardSection,

    -- * Grid Components
    grid,
    cardGrid,
    twoColumn,
    threeColumn,

    -- * Flex Components
    row,
    stack,
    stackToRow,
    spaceBetween,
    centered,

    -- * Visibility Components
    desktopOnly,
    mobileOnly,
    tabletUp,

    -- * Card Components
    card,
    cardWithHeader,
  )
where

--------------------------------------------------------------------------------

import Design.Lucid
  ( card,
    cardGrid,
    cardSection,
    cardWithHeader,
    centered,
    container,
    desktopOnly,
    grid,
    heroSection,
    mobileOnly,
    narrowContainer,
    row,
    section,
    spaceBetween,
    stack,
    stackToRow,
    tabletUp,
    threeColumn,
    twoColumn,
    wideContainer,
  )
