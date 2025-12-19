-- | Design system for KPBJ.
--
-- This is the main entry point for the styling library. Most templates
-- only need to import this module.
--
-- Example:
--
-- > import Design (styles, base, tablet, desktop)
-- > import Design.Tokens qualified as T
-- >
-- > cardStyles :: Text
-- > cardStyles = styles $ do
-- >   base [T.bgWhite, T.cardBorder, T.p4]
-- >   tablet [T.p6]
-- >   desktop [T.p8]
--
-- For design tokens (colors, spacing, typography), see "Design.Tokens".
--
-- For Lucid HTML components, see "Design.Lucid".
module Design
  ( -- * StyleBuilder DSL
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

    -- * Responsive Patterns (convenience re-exports)
    hideOnMobile,
    showOnMobile,
    hideOnDesktop,
    showOnDesktop,
    stackToRow,
    rowToStack,
    responsiveGrid,
  )
where

--------------------------------------------------------------------------------

import Design.Responsive
  ( hideOnDesktop,
    hideOnMobile,
    responsiveGrid,
    rowToStack,
    showOnDesktop,
    showOnMobile,
    stackToRow,
  )
import Design.StyleBuilder
  ( StyleBuilder,
    also,
    base,
    desktop,
    mobile,
    styles,
    tablet,
    ultrawide,
    unless,
    when,
    wide,
  )
