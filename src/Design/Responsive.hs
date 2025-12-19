-- | Responsive design patterns for Tailwind CSS.
--
-- This module provides pre-built responsive patterns for common layout needs.
-- These are composed class strings ready to use in templates.
--
-- For building custom responsive styles, use 'Design.StyleBuilder'.
module Design.Responsive
  ( -- * Visibility Patterns
    hideOnMobile,
    showOnMobile,
    hideOnDesktop,
    showOnDesktop,

    -- * Flex Direction Patterns
    stackToRow,
    rowToStack,

    -- * Grid Helpers
    gridCols,
    responsiveGrid,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Design.StyleBuilder.Internal (cls, lg, md)

--------------------------------------------------------------------------------
-- Visibility Patterns

-- | Hide an element on mobile, show on tablet and up.
hideOnMobile :: Text
hideOnMobile = "hidden md:block"

-- | Show an element only on mobile (hide on tablet and up).
showOnMobile :: Text
showOnMobile = "md:hidden"

-- | Hide an element on desktop (lg+), show on mobile\/tablet.
hideOnDesktop :: Text
hideOnDesktop = "lg:hidden"

-- | Show an element only on desktop (lg+).
showOnDesktop :: Text
showOnDesktop = "hidden lg:block"

--------------------------------------------------------------------------------
-- Flex Direction Patterns

-- | Stack vertically on mobile, row on tablet+.
stackToRow :: Text
stackToRow = "flex flex-col md:flex-row"

-- | Row on mobile, stack vertically on tablet+.
rowToStack :: Text
rowToStack = "flex flex-row md:flex-col"

--------------------------------------------------------------------------------
-- Grid Helpers

-- | Generate grid-cols-N class.
--
-- Example:
--
-- > gridCols 3
-- > -- Result: "grid-cols-3"
gridCols :: Int -> Text
gridCols n = "grid-cols-" <> Text.pack (show n)

-- | Generate a responsive grid class string.
--
-- Example:
--
-- > responsiveGrid 1 2 3
-- > -- Result: "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3"
responsiveGrid :: Int -> Int -> Int -> Text
responsiveGrid mobileCols tabletCols desktopCols =
  cls
    [ "grid",
      gridCols mobileCols,
      md (gridCols tabletCols),
      lg (gridCols desktopCols)
    ]
