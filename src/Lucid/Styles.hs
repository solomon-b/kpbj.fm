-- | DEPRECATED: Use "Design.StyleBuilder" instead.
--
-- This module is kept for backwards compatibility during migration.
-- It re-exports from the new Design.StyleBuilder module.
module Lucid.Styles
  ( -- * Core Builder
    StyleBuilder,
    class_',

    -- * Breakpoint Declarations
    base,
    mobile,
    tablet,
    desktop,
    wide,
    ultrawide,

    -- * Conditional Styles
    when,
    unless,

    -- * Additional Classes
    also,
  )
where

--------------------------------------------------------------------------------

import Design.StyleBuilder
  ( StyleBuilder,
    also,
    base,
    class_',
    desktop,
    mobile,
    tablet,
    ultrawide,
    unless,
    when,
    wide,
  )
