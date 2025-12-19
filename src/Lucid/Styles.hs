-- | DSL for building responsive Tailwind CSS class strings.
--
-- This module provides a builder pattern that separates base (mobile) styles
-- from responsive breakpoint overrides, making responsive design intentions explicit.
--
-- Example usage:
--
-- > cardStyles :: Text
-- > cardStyles = styles $ do
-- >   base [bgWhite, cardBorder, p4, mb4]
-- >   tablet [p6, mb6]
-- >   desktop [p8, mb8]
-- >
-- > -- Result: "bg-white border-2 border-gray-800 p-4 mb-4 md:p-6 md:mb-6 lg:p-8 lg:mb-8"
--
-- The DSL enforces mobile-first thinking by making the base (mobile) styles
-- the foundation, with breakpoint-specific overrides layered on top.
module Lucid.Styles
  ( -- * Core Builder
    StyleBuilder,
    styles,

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

import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Text (Text)
import Data.Text qualified as Text
import Lucid.Responsive qualified as Responsive

--------------------------------------------------------------------------------
-- Types

-- | A builder for accumulating responsive CSS classes.
--
-- Use with 'styles' to execute the builder and produce a class string.
type StyleBuilder = Writer [Text] ()

--------------------------------------------------------------------------------
-- Core Builder

-- | Execute a style builder and produce a space-separated class string.
--
-- Example:
--
-- > styles $ do
-- >   base ["bg-white", "p-4"]
-- >   tablet ["p-6"]
-- >   desktop ["p-8"]
-- >
-- > -- Result: "bg-white p-4 md:p-6 lg:p-8"
styles :: StyleBuilder -> Text
styles = Text.unwords . filter (not . Text.null) . execWriter

--------------------------------------------------------------------------------
-- Breakpoint Declarations

-- | Base styles that apply at all screen sizes (mobile-first foundation).
--
-- These are the default styles before any responsive overrides.
--
-- Example:
--
-- > base [bgWhite, cardBorder, p4, textSm]
base :: [Text] -> StyleBuilder
base = tell

-- | Alias for 'base' - styles that apply at all screen sizes.
--
-- Use when you want to emphasize these are the mobile defaults.
mobile :: [Text] -> StyleBuilder
mobile = base

-- | Tablet breakpoint styles (md: prefix, 768px+).
--
-- These override base styles on tablet-sized screens and larger.
--
-- Example:
--
-- > tablet [p6, textBase]
tablet :: [Text] -> StyleBuilder
tablet = tell . map Responsive.md

-- | Desktop breakpoint styles (lg: prefix, 1024px+).
--
-- These override base and tablet styles on desktop screens and larger.
--
-- Example:
--
-- > desktop [p8, textLg]
desktop :: [Text] -> StyleBuilder
desktop = tell . map Responsive.lg

-- | Wide screen breakpoint styles (xl: prefix, 1280px+).
--
-- For large desktop monitors.
--
-- Example:
--
-- > wide ["max-w-7xl"]
wide :: [Text] -> StyleBuilder
wide = tell . map Responsive.xl

-- | Ultra-wide screen breakpoint styles (2xl: prefix, 1536px+).
--
-- For very large displays.
--
-- Example:
--
-- > ultrawide ["max-w-8xl"]
ultrawide :: [Text] -> StyleBuilder
ultrawide = tell . map Responsive.xxl

--------------------------------------------------------------------------------
-- Conditional Styles

-- | Include styles only when a condition is true.
--
-- Example:
--
-- > styles $ do
-- >   base [bgWhite, p4]
-- >   when isActive $ base [bgBlue500]
-- >   when hasError $ base [borderRed500]
when :: Bool -> StyleBuilder -> StyleBuilder
when True builder = builder
when False _ = pure ()

-- | Include styles only when a condition is false.
--
-- Example:
--
-- > styles $ do
-- >   base [p4]
-- >   unless isCompact $ tablet [p6]
unless :: Bool -> StyleBuilder -> StyleBuilder
unless cond = when (not cond)

--------------------------------------------------------------------------------
-- Additional Classes

-- | Add arbitrary classes without a breakpoint prefix.
--
-- Useful for adding shared classes or classes that don't fit
-- the mobile/tablet/desktop pattern (like hover states).
--
-- Example:
--
-- > styles $ do
-- >   base [bgWhite, p4]
-- >   tablet [p6]
-- >   also ["hover:bg-gray-100", "transition-colors"]
also :: [Text] -> StyleBuilder
also = tell
