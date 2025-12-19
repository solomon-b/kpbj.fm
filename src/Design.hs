-- | Design system for KPBJ.
--
-- This is the main entry point for the styling library. Most templates
-- only need to import this module.
--
-- Example:
--
-- > import Design (class_, base, tablet, desktop)
-- > import Design.Tokens qualified as T
-- >
-- > -- Direct attribute usage (preferred)
-- > Lucid.div_ [class_ $ do
-- >   base [T.bgWhite, T.cardBorder, T.p4]
-- >   tablet [T.p6]
-- >   desktop [T.p8]]
--
-- For design tokens (colors, spacing, typography), see "Design.Tokens".
--
-- For Lucid HTML components, see "Design.Lucid".
module Design
  ( -- * StyleBuilder DSL
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
    class_,
    class_',
    desktop,
    mobile,
    tablet,
    ultrawide,
    unless,
    when,
    wide,
  )
