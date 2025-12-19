-- | Layout components for KPBJ templates.
--
-- These components encapsulate common responsive layout patterns,
-- making templates cleaner and ensuring consistency.
module Design.Lucid.Layout
  ( -- * Container Components
    container,
    narrowContainer,
    wideContainer,

    -- * Section Components
    section,
    heroSection,
    cardSection,

    -- * Grid Components
    grid,
    cardGrid,
    twoColumn,
    threeColumn,

    -- * Flex Components
    row,
    stack,
    stackToRow,
    spaceBetween,
    centered,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Design.Responsive qualified as Responsive
import Design.StyleBuilder.Internal (cls, md)
import Design.Tokens qualified as Tokens
import Lucid qualified

--------------------------------------------------------------------------------
-- Container Components

-- | Standard page container with max-width and responsive padding.
container :: Lucid.Html () -> Lucid.Html ()
container = Lucid.div_ [Lucid.class_ Tokens.containerWidth]

-- | Narrow container for content-focused pages (articles, forms).
narrowContainer :: Lucid.Html () -> Lucid.Html ()
narrowContainer = Lucid.div_ [Lucid.class_ $ cls ["max-w-3xl", "mx-auto", Tokens.containerPadding]]

-- | Wide container for dashboard-style layouts.
wideContainer :: Lucid.Html () -> Lucid.Html ()
wideContainer = Lucid.div_ [Lucid.class_ $ cls ["max-w-7xl", "mx-auto", Tokens.containerPadding]]

--------------------------------------------------------------------------------
-- Section Components

-- | Standard page section with border and padding.
section :: Lucid.Html () -> Lucid.Html ()
section = Lucid.section_ [Lucid.class_ Tokens.sectionBase]

-- | Hero section for page headers (centered text, larger padding).
heroSection :: Lucid.Html () -> Lucid.Html ()
heroSection =
  Lucid.section_
    [ Lucid.class_ $
        cls
          [ Tokens.bgWhite,
            Tokens.cardBorder,
            Tokens.p6,
            md Tokens.p8,
            Tokens.mb6,
            md Tokens.mb8,
            Tokens.fullWidth,
            "text-center"
          ]
    ]

-- | Card-style section (for forms, callouts).
cardSection :: Lucid.Html () -> Lucid.Html ()
cardSection = Lucid.div_ [Lucid.class_ Tokens.cardBase]

--------------------------------------------------------------------------------
-- Grid Components

-- | Flexible grid with responsive columns.
--
-- Example:
--
-- > grid 1 2 3 gap4 content
-- > -- 1 column on mobile, 2 on tablet, 3 on desktop
grid :: Int -> Int -> Int -> Text -> Lucid.Html () -> Lucid.Html ()
grid mobileCols tabletCols desktopCols gapClass =
  Lucid.div_ [Lucid.class_ $ cls [Responsive.responsiveGrid mobileCols tabletCols desktopCols, gapClass]]

-- | Grid optimized for card layouts (consistent gap, full width).
cardGrid :: Int -> Int -> Int -> Lucid.Html () -> Lucid.Html ()
cardGrid mobileCols tabletCols desktopCols =
  Lucid.div_
    [ Lucid.class_ $
        cls
          [ Responsive.responsiveGrid mobileCols tabletCols desktopCols,
            Tokens.gap4,
            md Tokens.gap6,
            Tokens.mb6,
            md Tokens.mb8,
            Tokens.fullWidth
          ]
    ]

-- | Two-column layout (stacks on mobile).
twoColumn :: Lucid.Html () -> Lucid.Html () -> Lucid.Html ()
twoColumn left right =
  Lucid.div_ [Lucid.class_ $ cls [Responsive.responsiveGrid 1 2 2, Tokens.gap6]] $ do
    Lucid.div_ left
    Lucid.div_ right

-- | Three-column layout (stacks on mobile, 2 cols on tablet).
threeColumn :: Lucid.Html () -> Lucid.Html () -> Lucid.Html () -> Lucid.Html ()
threeColumn col1 col2 col3 =
  Lucid.div_ [Lucid.class_ $ cls [Responsive.responsiveGrid 1 2 3, Tokens.gap6]] $ do
    Lucid.div_ col1
    Lucid.div_ col2
    Lucid.div_ col3

--------------------------------------------------------------------------------
-- Flex Components

-- | Horizontal row with gap.
row :: Text -> Lucid.Html () -> Lucid.Html ()
row gapClass = Lucid.div_ [Lucid.class_ $ cls ["flex", "flex-row", "items-center", gapClass]]

-- | Vertical stack with gap.
stack :: Text -> Lucid.Html () -> Lucid.Html ()
stack gapClass = Lucid.div_ [Lucid.class_ $ cls ["flex", "flex-col", gapClass]]

-- | Stack on mobile, row on tablet+.
stackToRow :: Text -> Lucid.Html () -> Lucid.Html ()
stackToRow gapClass = Lucid.div_ [Lucid.class_ $ cls [Responsive.stackToRow, "items-start", md "items-center", gapClass]]

-- | Flex row with space-between alignment.
spaceBetween :: Lucid.Html () -> Lucid.Html ()
spaceBetween = Lucid.div_ [Lucid.class_ "flex flex-wrap items-center justify-between gap-4"]

-- | Centered content (both axes).
centered :: Lucid.Html () -> Lucid.Html ()
centered = Lucid.div_ [Lucid.class_ "flex items-center justify-center"]
