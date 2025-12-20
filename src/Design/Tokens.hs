-- | Design system tokens for KPBJ.
--
-- This module defines the visual language of the site as reusable constants.
-- Using these tokens ensures consistency across all templates.
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
    borderGray800,
    borderGray400,
    cardBorder,

    -- * Colors
    bgWhite,
    bgGray100,
    bgGray800,
    bgGray900,
    textGray600,
    textGray700,
    textGray800,
    textWhite,

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

borderGray800 :: Text
borderGray800 = "border-gray-800"

borderGray400 :: Text
borderGray400 = "border-gray-400"

-- | Standard card border style.
cardBorder :: Text
cardBorder = cls [border2, borderGray800]

--------------------------------------------------------------------------------
-- Colors

bgWhite :: Text
bgWhite = "bg-white"

bgGray100 :: Text
bgGray100 = "bg-gray-100"

bgGray800 :: Text
bgGray800 = "bg-gray-800"

bgGray900 :: Text
bgGray900 = "bg-gray-900"

textGray600 :: Text
textGray600 = "text-gray-600"

textGray700 :: Text
textGray700 = "text-gray-700"

textGray800 :: Text
textGray800 = "text-gray-800"

textWhite :: Text
textWhite = "text-white"

--------------------------------------------------------------------------------
-- Semantic Colors (Notifications)

successBg :: Text
successBg = "bg-green-100"

successBorder :: Text
successBorder = "border-green-600"

successText :: Text
successText = "text-green-800"

errorBg :: Text
errorBg = "bg-red-100"

errorBorder :: Text
errorBorder = "border-red-600"

errorText :: Text
errorText = "text-red-800"

warningBg :: Text
warningBg = "bg-yellow-100"

warningBorder :: Text
warningBorder = "border-yellow-600"

warningText :: Text
warningText = "text-yellow-800"

infoBg :: Text
infoBg = "bg-blue-100"

infoBorder :: Text
infoBorder = "border-blue-600"

infoText :: Text
infoText = "text-blue-800"

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
buttonSecondary = cls [bgWhite, textGray800, cardBorder, px4, py2, fontBold, "hover:bg-gray-100"]

-- | Text link styling.
linkText :: Text
linkText = "text-blue-600 hover:text-blue-800 hover:underline"

-- | Navigation link styling.
navLink :: Text
navLink = cls [fontBold, "uppercase", "hover:underline"]
