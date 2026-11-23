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
  ( -- * Typography
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
    fontSemibold,

    -- * Spacing Scale
    gap1,
    gap2,
    gap3,
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
    mt2,
    mt4,
    mt6,
    mt8,
    pb2,

    -- * Responsive Spacing
    sectionPadding,
    cardPadding,
    containerPadding,

    -- * Borders
    border2,
    borderMuted,
    borderDefault,
    cardBorder,

    -- * Background Colors
    bgMain,
    bgAlt,
    bgButton,
    bgInverse,

    -- * Text Colors
    fgPrimary,
    fgMuted,
    fgInverse,

    -- * Hover Colors
    hoverBg,

    -- * Status Colors
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
    mutedBg,

    -- * Layout
    maxWidth,
    containerWidth,
    fullWidth,

    -- * Component Patterns
    cardBase,
    buttonPrimary,
    linkText,
    navLink,

    -- * Responsive Visibility
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
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Design.Theme qualified as Theme
import Lucid.Tailwind (cls, gridCols, lg, md, responsiveGrid)

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
metaText = cls [textXs, md textSm, fgMuted]

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

fontSemibold :: Text
fontSemibold = "font-semibold"

--------------------------------------------------------------------------------
-- Spacing Scale

gap1 :: Text
gap1 = "gap-1"

gap2 :: Text
gap2 = "gap-2"

gap3 :: Text
gap3 = "gap-3"

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

mt2 :: Text
mt2 = "mt-2"

mt4 :: Text
mt4 = "mt-4"

mt6 :: Text
mt6 = "mt-6"

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
-- Borders

border2 :: Text
border2 = "border-2"

-- | Muted border for subtle dividers and inputs.
borderMuted :: Text
borderMuted = Theme.borderMuted

-- | Default border for cards and sections.
borderDefault :: Text
borderDefault = Theme.borderDefault

-- | Standard card border style (2px + default color).
cardBorder :: Text
cardBorder = cls [border2, borderDefault]

--------------------------------------------------------------------------------
-- Background Colors

-- | Main page background.
bgMain :: Text
bgMain = Theme.bgMain

-- | Alternate background for cards and muted sections.
bgAlt :: Text
bgAlt = Theme.bgAlt

-- | Background for buttons. It differs from the row hover color.
bgButton :: Text
bgButton = Theme.bgButton

-- | Inverse background (dark on light, light on dark).
bgInverse :: Text
bgInverse = Theme.bgInverse

--------------------------------------------------------------------------------
-- Text Colors

-- | Primary text color.
fgPrimary :: Text
fgPrimary = Theme.fgPrimary

-- | Muted/secondary text color.
fgMuted :: Text
fgMuted = Theme.fgMuted

-- | Inverse text (for dark backgrounds).
fgInverse :: Text
fgInverse = Theme.fgInverse

--------------------------------------------------------------------------------
-- Hover Colors

-- | Hover background color.
hoverBg :: Text
hoverBg = Theme.hoverBg

--------------------------------------------------------------------------------
-- Status Colors
--
-- Each @*Bg@ token is a 10% tint of its status color. Tailwind cannot apply
-- an opacity modifier such as @/10@ to a color from a CSS variable, and it
-- writes no rule for one. A mix with transparent gives the tint.

successBg :: Text
successBg = "bg-[color-mix(in_srgb,var(--theme-success)_10%,transparent)]"

successBorder :: Text
successBorder = "border-[var(--theme-success)]"

successText :: Text
successText = Theme.success

errorBg :: Text
errorBg = "bg-[color-mix(in_srgb,var(--theme-error)_10%,transparent)]"

errorBorder :: Text
errorBorder = "border-[var(--theme-error)]"

errorText :: Text
errorText = Theme.error

warningBg :: Text
warningBg = "bg-[color-mix(in_srgb,var(--theme-warning)_10%,transparent)]"

warningBorder :: Text
warningBorder = "border-[var(--theme-warning)]"

warningText :: Text
warningText = Theme.warning

infoBg :: Text
infoBg = "bg-[color-mix(in_srgb,var(--theme-info)_10%,transparent)]"

infoBorder :: Text
infoBorder = "border-[var(--theme-info)]"

infoText :: Text
infoText = Theme.info

-- | A tint of the muted foreground, for a neutral status.
mutedBg :: Text
mutedBg = "bg-[color-mix(in_srgb,var(--theme-fg-muted)_10%,transparent)]"

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
-- Component Patterns

-- | Base card styling.
cardBase :: Text
cardBase = cls [bgMain]

-- | Primary button styling.
buttonPrimary :: Text
buttonPrimary = cls [Theme.accent, Theme.accentFg, Theme.accentHover, px4, md px6, py2, md "py-3", fontBold]

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
