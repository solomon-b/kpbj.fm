-- | Responsive design utilities for Tailwind CSS.
--
-- This module provides combinators for building responsive class strings
-- that work with Tailwind's mobile-first breakpoint system.
--
-- Tailwind breakpoints:
--   - (none) : All screen sizes (mobile-first base)
--   - sm:    : 640px and up
--   - md:    : 768px and up
--   - lg:    : 1024px and up
--   - xl:    : 1280px and up
--   - 2xl:   : 1536px and up
module Lucid.Responsive
  ( -- * Breakpoint Prefixers
    sm,
    md,
    lg,
    xl,
    xxl,

    -- * Class Combinators
    cls,
    clsWhen,

    -- * Common Responsive Patterns
    hideOnMobile,
    showOnMobile,
    hideOnDesktop,
    showOnDesktop,
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

--------------------------------------------------------------------------------
-- Breakpoint Prefixers

-- | Prefix a class with 'sm:' breakpoint (640px+)
sm :: Text -> Text
sm c = "sm:" <> c

-- | Prefix a class with 'md:' breakpoint (768px+)
md :: Text -> Text
md c = "md:" <> c

-- | Prefix a class with 'lg:' breakpoint (1024px+)
lg :: Text -> Text
lg c = "lg:" <> c

-- | Prefix a class with 'xl:' breakpoint (1280px+)
xl :: Text -> Text
xl c = "xl:" <> c

-- | Prefix a class with '2xl:' breakpoint (1536px+)
xxl :: Text -> Text
xxl c = "2xl:" <> c

--------------------------------------------------------------------------------
-- Class Combinators

-- | Combine a list of class strings into a single space-separated string.
--
-- Example:
--
-- > cls ["text-sm", md "text-base", lg "text-lg"]
-- > -- Result: "text-sm md:text-base lg:text-lg"
cls :: [Text] -> Text
cls = Text.unwords . filter (not . Text.null)

-- | Include a class only if the condition is True.
--
-- Example:
--
-- > cls ["p-4", clsWhen isActive "bg-blue-500"]
clsWhen :: Bool -> Text -> Text
clsWhen True c = c
clsWhen False _ = ""

--------------------------------------------------------------------------------
-- Common Responsive Patterns

-- | Hide an element on mobile, show on tablet and up.
hideOnMobile :: Text
hideOnMobile = "hidden md:block"

-- | Show an element only on mobile (hide on tablet and up).
showOnMobile :: Text
showOnMobile = "md:hidden"

-- | Hide an element on desktop (lg+), show on mobile/tablet.
hideOnDesktop :: Text
hideOnDesktop = "lg:hidden"

-- | Show an element only on desktop (lg+).
showOnDesktop :: Text
showOnDesktop = "hidden lg:block"

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
responsiveGrid mobile tabletCols desktopCols =
  cls
    [ "grid",
      gridCols mobile,
      md (gridCols tabletCols),
      lg (gridCols desktopCols)
    ]
