-- | Design system tokens for KPBJ.
--
-- This module defines the visual language of the site as reusable constants.
-- Using these tokens ensures consistency across all templates.
--
-- = Color System
--
-- Color tokens are powered by the centralized theme system in "Design.Theme".
-- Colors are defined as CSS custom properties, allowing runtime theme switching
-- and automatic light/dark mode support without @dark:@ prefixes.
--
-- See "Design.Theme" for the underlying implementation and available themes.
module Design.Tokens
  ( -- * Responsive Visibility
    hideOnMobile,
    showOnMobile,
    hideOnDesktop,
    showOnDesktop,

    -- * Responsive Flex
    stackToRow,
    rowToStack,

    -- * Responsive Grid
    gridCols,
    responsiveGrid,

    -- * Typography
    textXs,
    textSm,
    textBase,
    textLg,
    textXl,
    text2xl,
    text3xl,

    -- * Responsive Typography
    bodyText,
    metaText,
    headingLg,
    headingXl,
    heading2xl,

    -- * Font Weights
    fontBold,
    fontNormal,
    fontMedium,

    -- * Spacing Scale
    gap2,
    gap4,
    gap6,
    gap8,
    p2,
    p3,
    p4,
    p6,
    p8,
    px3,
    px4,
    px6,
    px8,
    py2,
    py4,
    py6,
    py8,
    mb2,
    mb4,
    mb6,
    mb8,
    mt4,
    mt8,
    pb2,

    -- * Responsive Spacing
    sectionPadding,
    cardPadding,
    containerPadding,

    -- * Borders
    border2,
    borderGray200,
    borderGray300,
    borderGray400,
    borderGray600,
    borderGray800,
    cardBorder,

    -- * Colors (Theme-Aware)

    --
    -- These tokens automatically adapt to light/dark mode via CSS custom properties.
    -- No @dark:@ prefixes needed - the theme handles mode switching.
    bgWhite,
    bgGray50,
    bgGray100,
    bgGray200,
    bgGray300,
    bgGray800,
    bgGray900,
    textGray400,
    textGray500,
    textGray600,
    textGray700,
    textGray800,
    textGray900,
    textWhite,

    -- * Hover Colors
    hoverBgGray50,
    hoverBgGray100,
    hoverBgGray200,
    hoverBgGray700,

    -- * Semantic Colors (Notifications)

    --
    -- Theme-aware status colors for banners, alerts, and feedback.
    successBg,
    successBorder,
    successText,
    errorBg,
    errorBorder,
    errorText,
    warningBg,
    warningBorder,
    warningText,
    infoBg,
    infoBorder,
    infoText,

    -- * Layout
    maxWidth,
    containerWidth,
    fullWidth,

    -- * Common Component Patterns
    cardBase,
    sectionBase,
    buttonPrimary,
    buttonSecondary,
    linkText,
    navLink,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Design.StyleBuilder.Internal (cls, lg, md)
import Design.Theme qualified as Theme

--------------------------------------------------------------------------------
-- Typography

textXs :: Text
textXs = "text-xs"

textSm :: Text
textSm = "text-sm"

textBase :: Text
textBase = "text-base"

textLg :: Text
textLg = "text-lg"

textXl :: Text
textXl = "text-xl"

text2xl :: Text
text2xl = "text-2xl"

text3xl :: Text
text3xl = "text-3xl"

--------------------------------------------------------------------------------
-- Responsive Typography

-- | Body text that scales up on larger screens.
bodyText :: Text
bodyText = cls [textSm, md textBase]

-- | Small metadata text (dates, categories, etc.)
metaText :: Text
metaText = cls [textXs, md textSm, textGray600]

-- | Large heading (sections)
headingLg :: Text
headingLg = cls [textLg, md textXl, fontBold]

-- | XL heading (page titles)
headingXl :: Text
headingXl = cls [textXl, md text2xl, fontBold]

-- | 2XL heading (hero sections)
heading2xl :: Text
heading2xl = cls [text2xl, md text3xl, fontBold]

--------------------------------------------------------------------------------
-- Font Weights

fontBold :: Text
fontBold = "font-bold"

fontNormal :: Text
fontNormal = "font-normal"

fontMedium :: Text
fontMedium = "font-medium"

--------------------------------------------------------------------------------
-- Spacing Scale

gap2 :: Text
gap2 = "gap-2"

gap4 :: Text
gap4 = "gap-4"

gap6 :: Text
gap6 = "gap-6"

gap8 :: Text
gap8 = "gap-8"

p2 :: Text
p2 = "p-2"

p3 :: Text
p3 = "p-3"

p4 :: Text
p4 = "p-4"

p6 :: Text
p6 = "p-6"

p8 :: Text
p8 = "p-8"

px3 :: Text
px3 = "px-3"

px4 :: Text
px4 = "px-4"

px6 :: Text
px6 = "px-6"

px8 :: Text
px8 = "px-8"

py2 :: Text
py2 = "py-2"

py4 :: Text
py4 = "py-4"

py6 :: Text
py6 = "py-6"

py8 :: Text
py8 = "py-8"

mb2 :: Text
mb2 = "mb-2"

mb4 :: Text
mb4 = "mb-4"

mb6 :: Text
mb6 = "mb-6"

mb8 :: Text
mb8 = "mb-8"

mt4 :: Text
mt4 = "mt-4"

mt8 :: Text
mt8 = "mt-8"

pb2 :: Text
pb2 = "pb-2"

--------------------------------------------------------------------------------
-- Responsive Spacing

-- | Vertical padding for page sections.
sectionPadding :: Text
sectionPadding = cls [py6, md py8, lg "py-12"]

-- | Padding for cards and contained elements.
cardPadding :: Text
cardPadding = cls [p4, md p6]

-- | Horizontal padding for the main container.
containerPadding :: Text
containerPadding = cls [px4, md px6, lg px8]

--------------------------------------------------------------------------------
-- Borders (Theme-Aware)

border2 :: Text
border2 = "border-2"

-- | Muted border (subtle dividers, inputs)
borderGray200 :: Text
borderGray200 = Theme.borderMuted

-- | Muted border (alias)
borderGray300 :: Text
borderGray300 = Theme.borderMuted

-- | Medium border
borderGray400 :: Text
borderGray400 = Theme.borderMuted

-- | Default border (cards, sections)
borderGray600 :: Text
borderGray600 = Theme.borderDefault

-- | Strong border (brutalist style)
borderGray800 :: Text
borderGray800 = Theme.borderDefault

-- | Standard card border style.
cardBorder :: Text
cardBorder = cls [border2, Theme.borderDefault]

--------------------------------------------------------------------------------
-- Colors (Theme-Aware via CSS Custom Properties)
--
-- These tokens use the centralized theme system. Colors automatically adapt
-- to light/dark mode without requiring dark: prefixes in templates.
-- The theme CSS variables are set in Component.Frame via Design.Theme.themeCSS.

-- | Main background - white in light mode, dark gray in dark mode
bgWhite :: Text
bgWhite = Theme.bgMain

-- | Subtle card background
bgGray50 :: Text
bgGray50 = Theme.bgAlt

-- | Muted section background
bgGray100 :: Text
bgGray100 = Theme.bgAlt

-- | Secondary muted background
bgGray200 :: Text
bgGray200 = Theme.bgAlt

-- | Tertiary muted background
bgGray300 :: Text
bgGray300 = Theme.bgAlt

-- | Inverse/dark background
bgGray800 :: Text
bgGray800 = Theme.bgInverse

-- | Deep inverse background
bgGray900 :: Text
bgGray900 = Theme.bgInverse

-- | Muted/disabled text
textGray400 :: Text
textGray400 = Theme.fgMuted

-- | Muted text (alias)
textGray500 :: Text
textGray500 = Theme.fgMuted

-- | Secondary muted text
textGray600 :: Text
textGray600 = Theme.fgMuted

-- | Secondary text
textGray700 :: Text
textGray700 = Theme.fgMuted

-- | Primary text
textGray800 :: Text
textGray800 = Theme.fgPrimary

-- | Strong primary text
textGray900 :: Text
textGray900 = Theme.fgPrimary

-- | Inverse text (for dark backgrounds)
textWhite :: Text
textWhite = Theme.fgInverse

-- Hover backgrounds
hoverBgGray50 :: Text
hoverBgGray50 = Theme.hoverBg

hoverBgGray100 :: Text
hoverBgGray100 = Theme.hoverBg

hoverBgGray200 :: Text
hoverBgGray200 = Theme.hoverBg

hoverBgGray700 :: Text
hoverBgGray700 = Theme.hoverBg

--------------------------------------------------------------------------------
-- Semantic Colors (Notifications)
--
-- Theme-aware status colors for banners, alerts, and feedback.
-- These automatically adapt to light/dark mode.
-- For flexibility, we provide text, bg, and border variants using the same base color.

successBg :: Text
successBg = "bg-[var(--theme-success)]/10"

successBorder :: Text
successBorder = "border-[var(--theme-success)]"

successText :: Text
successText = Theme.success

errorBg :: Text
errorBg = "bg-[var(--theme-error)]/10"

errorBorder :: Text
errorBorder = "border-[var(--theme-error)]"

errorText :: Text
errorText = Theme.error

warningBg :: Text
warningBg = "bg-[var(--theme-warning)]/10"

warningBorder :: Text
warningBorder = "border-[var(--theme-warning)]"

warningText :: Text
warningText = Theme.warning

infoBg :: Text
infoBg = "bg-[var(--theme-info)]/10"

infoBorder :: Text
infoBorder = "border-[var(--theme-info)]"

infoText :: Text
infoText = Theme.info

--------------------------------------------------------------------------------
-- Layout

-- | Maximum content width.
maxWidth :: Text
maxWidth = "max-w-6xl"

-- | Full container width setup.
containerWidth :: Text
containerWidth = cls [maxWidth, "mx-auto", containerPadding]

-- | Full width element.
fullWidth :: Text
fullWidth = "w-full"

--------------------------------------------------------------------------------
-- Common Component Patterns

-- | Base card styling (white bg, border, padding).
cardBase :: Text
cardBase = cls [bgWhite]

-- | Base section styling (white bg, border, padding, margin).
sectionBase :: Text
sectionBase = cls [bgWhite, cardBorder, p6, md p8, mb6, md mb8, fullWidth]

-- | Primary button styling.
buttonPrimary :: Text
buttonPrimary = cls [Theme.accent, Theme.accentFg, Theme.accentHover, px4, md px6, py2, md "py-3", fontBold]

-- | Secondary button styling.
buttonSecondary :: Text
buttonSecondary = cls [bgWhite, textGray800, cardBorder, px4, py2, fontBold, Theme.hoverBg]

-- | Text link styling.
linkText :: Text
linkText = "text-[var(--theme-info)] hover:underline"

-- | Navigation link styling.
navLink :: Text
navLink = cls [fontBold, "uppercase", "hover:underline"]

--------------------------------------------------------------------------------
-- Responsive Visibility

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
-- Responsive Flex

-- | Stack vertically on mobile, row on tablet+.
stackToRow :: Text
stackToRow = "flex flex-col md:flex-row"

-- | Row on mobile, stack vertically on tablet+.
rowToStack :: Text
rowToStack = "flex flex-row md:flex-col"

--------------------------------------------------------------------------------
-- Responsive Grid

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
