-- | Internal utilities for building Tailwind CSS class strings.
--
-- This module re-exports combinators from "Lucid.Tailwind" for backwards
-- compatibility. New code should import "Lucid.Tailwind" directly.
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

import Lucid.Tailwind (cls, clsWhen, lg, md, sm, xl, xxl)
