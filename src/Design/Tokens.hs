-- | Design system tokens for KPBJ.
--
-- This module defines the visual language of the site as reusable constants.
-- Using these tokens ensures consistency across all templates.
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

    -- * Colors
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

borderGray200 :: Text
borderGray200 = "border-gray-200 dark:border-gray-600"

borderGray300 :: Text
borderGray300 = "border-gray-300 dark:border-gray-600"

borderGray400 :: Text
borderGray400 = "border-gray-400 dark:border-gray-500"

borderGray600 :: Text
borderGray600 = "border-gray-600 dark:border-gray-500"

borderGray800 :: Text
borderGray800 = "border-gray-800 dark:border-gray-600"

-- | Standard card border style.
cardBorder :: Text
cardBorder = cls [border2, borderGray800]

--------------------------------------------------------------------------------
-- Colors (with dark mode variants)

bgWhite :: Text
bgWhite = "bg-white dark:bg-gray-800"

bgGray50 :: Text
bgGray50 = "bg-gray-50 dark:bg-gray-700"

bgGray100 :: Text
bgGray100 = "bg-gray-100 dark:bg-gray-700"

bgGray200 :: Text
bgGray200 = "bg-gray-200 dark:bg-gray-600"

bgGray300 :: Text
bgGray300 = "bg-gray-300 dark:bg-gray-600"

bgGray800 :: Text
bgGray800 = "bg-gray-800 dark:bg-gray-900"

bgGray900 :: Text
bgGray900 = "bg-gray-900"

textGray400 :: Text
textGray400 = "text-gray-400 dark:text-gray-500"

textGray500 :: Text
textGray500 = "text-gray-500 dark:text-gray-400"

textGray600 :: Text
textGray600 = "text-gray-600 dark:text-gray-400"

textGray700 :: Text
textGray700 = "text-gray-700 dark:text-gray-300"

textGray800 :: Text
textGray800 = "text-gray-800 dark:text-gray-200"

textGray900 :: Text
textGray900 = "text-gray-900 dark:text-gray-100"

textWhite :: Text
textWhite = "text-white"

-- Hover backgrounds
hoverBgGray50 :: Text
hoverBgGray50 = "hover:bg-gray-50 dark:hover:bg-gray-700"

hoverBgGray100 :: Text
hoverBgGray100 = "hover:bg-gray-100 dark:hover:bg-gray-700"

hoverBgGray200 :: Text
hoverBgGray200 = "hover:bg-gray-200 dark:hover:bg-gray-600"

hoverBgGray700 :: Text
hoverBgGray700 = "hover:bg-gray-700 dark:hover:bg-gray-600"

--------------------------------------------------------------------------------
-- Semantic Colors (Notifications, with dark mode variants)

successBg :: Text
successBg = "bg-green-100 dark:bg-green-900"

successBorder :: Text
successBorder = "border-green-600 dark:border-green-500"

successText :: Text
successText = "text-green-800 dark:text-green-200"

errorBg :: Text
errorBg = "bg-red-100 dark:bg-red-900"

errorBorder :: Text
errorBorder = "border-red-600 dark:border-red-500"

errorText :: Text
errorText = "text-red-800 dark:text-red-200"

warningBg :: Text
warningBg = "bg-yellow-100 dark:bg-yellow-900"

warningBorder :: Text
warningBorder = "border-yellow-600 dark:border-yellow-500"

warningText :: Text
warningText = "text-yellow-800 dark:text-yellow-200"

infoBg :: Text
infoBg = "bg-blue-100 dark:bg-blue-900"

infoBorder :: Text
infoBorder = "border-blue-600 dark:border-blue-500"

infoText :: Text
infoText = "text-blue-800 dark:text-blue-200"

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
buttonPrimary = cls [bgGray800, textWhite, px4, md px6, py2, md "py-3", fontBold, "hover:bg-gray-700"]

-- | Secondary button styling.
buttonSecondary :: Text
buttonSecondary = cls [bgWhite, textGray800, cardBorder, px4, py2, fontBold, "hover:bg-gray-100 dark:hover:bg-gray-700"]

-- | Text link styling.
linkText :: Text
linkText = "text-blue-600 dark:text-blue-400 hover:text-blue-800 dark:hover:text-blue-300 hover:underline"

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
