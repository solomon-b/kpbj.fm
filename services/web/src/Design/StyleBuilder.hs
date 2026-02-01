-- | DSL for building responsive Tailwind CSS class strings.
--
-- This module provides a builder pattern that separates base (mobile) styles
-- from responsive breakpoint overrides, making responsive design intentions explicit.
--
-- Example usage:
--
-- > -- Direct attribute usage (preferred)
-- > Lucid.div_ [className $ do
-- >   base [bgWhite, cardBorder, p4, mb4]
-- >   tablet [p6, mb6]
-- >   desktop [p8, mb8]]
-- >
-- > -- Or with raw Text (for string interpolation)
-- > cardStyles :: Text
-- > cardStyles = class_' $ base [bgWhite, p4]
--
-- The DSL enforces mobile-first thinking by making the base (mobile) styles
-- the foundation, with breakpoint-specific overrides layered on top.
module Design.StyleBuilder
  ( -- * Core Builder
    StyleBuilder,
    class_,
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

import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Text (Text)
import Data.Text qualified as Text
import Design.StyleBuilder.Internal qualified as Internal
import Lucid qualified
import Lucid.Base (Attributes)

--------------------------------------------------------------------------------
-- Types

-- | A builder for accumulating responsive CSS classes.
--
-- Use with 'styles' to execute the builder and produce a class string.
type StyleBuilder = Writer [Text] ()

--------------------------------------------------------------------------------
-- Core Builder

-- | Execute a style builder and produce a Lucid class attribute.
--
-- This is the preferred way to use the style builder in templates.
--
-- Example:
--
-- > Lucid.div_ [class_ $ do
-- >   base ["bg-white", "p-4"]
-- >   tablet ["p-6"]
-- >   desktop ["p-8"]]
class_ :: StyleBuilder -> Attributes
class_ = Lucid.class_ . class_'

-- | Execute a style builder and produce a space-separated class string.
--
-- Use this when you need raw Text, such as in string interpolation.
--
-- Example:
--
-- > class_' $ do
-- >   base ["bg-white", "p-4"]
-- >   tablet ["p-6"]
-- >   desktop ["p-8"]
-- >
-- > -- Result: "bg-white p-4 md:p-6 lg:p-8"
class_' :: StyleBuilder -> Text
class_' = Text.unwords . filter (not . Text.null) . execWriter

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
tablet = tell . map Internal.md

-- | Desktop breakpoint styles (lg: prefix, 1024px+).
--
-- These override base and tablet styles on desktop screens and larger.
--
-- Example:
--
-- > desktop [p8, textLg]
desktop :: [Text] -> StyleBuilder
desktop = tell . map Internal.lg

-- | Wide screen breakpoint styles (xl: prefix, 1280px+).
--
-- For large desktop monitors.
--
-- Example:
--
-- > wide ["max-w-7xl"]
wide :: [Text] -> StyleBuilder
wide = tell . map Internal.xl

-- | Ultra-wide screen breakpoint styles (2xl: prefix, 1536px+).
--
-- For very large displays.
--
-- Example:
--
-- > ultrawide ["max-w-8xl"]
ultrawide :: [Text] -> StyleBuilder
ultrawide = tell . map Internal.xxl

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
-- the mobile\/tablet\/desktop pattern (like hover states).
--
-- Example:
--
-- > styles $ do
-- >   base [bgWhite, p4]
-- >   tablet [p6]
-- >   also ["hover:bg-gray-100", "transition-colors"]
also :: [Text] -> StyleBuilder
also = tell
