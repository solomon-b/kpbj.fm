-- | Internal utilities for building Tailwind CSS class strings.
--
-- This module provides low-level primitives for class string manipulation.
-- For most use cases, prefer the higher-level 'Design.StyleBuilder' DSL.
--
-- These functions are exposed for:
--
--   * Building design tokens that need responsive prefixes
--   * Gradual migration from direct @cls@ usage to the StyleBuilder DSL
--   * Advanced use cases requiring fine-grained control
module Design.StyleBuilder.Internal
  ( -- * Class Combinators
    cls,
    clsWhen,

    -- * Breakpoint Prefixers
    sm,
    md,
    lg,
    xl,
    xxl,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------
-- Class Combinators

-- | Combine a list of class strings into a single space-separated string.
--
-- Empty strings are filtered out automatically.
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
-- Breakpoint Prefixers

-- | Prefix a class with @sm:@ breakpoint (640px+).
sm :: Text -> Text
sm c = "sm:" <> c

-- | Prefix a class with @md:@ breakpoint (768px+).
md :: Text -> Text
md c = "md:" <> c

-- | Prefix a class with @lg:@ breakpoint (1024px+).
lg :: Text -> Text
lg c = "lg:" <> c

-- | Prefix a class with @xl:@ breakpoint (1280px+).
xl :: Text -> Text
xl c = "xl:" <> c

-- | Prefix a class with @2xl:@ breakpoint (1536px+).
xxl :: Text -> Text
xxl c = "2xl:" <> c
