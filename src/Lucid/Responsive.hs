-- | DEPRECATED: Use "Design.StyleBuilder.Internal" and "Design.Responsive" instead.
--
-- This module is kept for backwards compatibility during migration.
-- It re-exports from the new Design.* modules.
module Lucid.Responsive
  ( -- * Breakpoint Prefixers (from Design.StyleBuilder.Internal)
    sm,
    md,
    lg,
    xl,
    xxl,

    -- * Class Combinators (from Design.StyleBuilder.Internal)
    cls,
    clsWhen,

    -- * Common Responsive Patterns (from Design.Responsive)
    hideOnMobile,
    showOnMobile,
    hideOnDesktop,
    showOnDesktop,
    stackToRow,
    rowToStack,

    -- * Grid Helpers (from Design.Responsive)
    gridCols,
    responsiveGrid,
  )
where

--------------------------------------------------------------------------------

import Design.Responsive
  ( gridCols,
    hideOnDesktop,
    hideOnMobile,
    responsiveGrid,
    rowToStack,
    showOnDesktop,
    showOnMobile,
    stackToRow,
  )
import Design.StyleBuilder.Internal
  ( cls,
    clsWhen,
    lg,
    md,
    sm,
    xl,
    xxl,
  )
