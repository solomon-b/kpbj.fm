-- | Tailwind CSS utilities for Lucid2 templates.
--
-- This module provides type-safe combinators for building Tailwind CSS class
-- strings. It helps prevent typos in variant prefixes and provides a clean
-- API for responsive and stateful styling.
--
-- = Quick Start
--
-- @
-- import Lucid
-- import Lucid.Tailwind
--
-- myButton :: Html ()
-- myButton =
--   button_
--     [ class_ $ cls
--         [ "px-4 py-2 rounded"
--         , "bg-blue-500 text-white"
--         , hover "bg-blue-600"
--         , focus "ring-2 ring-blue-300"
--         , disabled "opacity-50 cursor-not-allowed"
--         , md "px-6 py-3"
--         ]
--     ]
--     "Click me"
-- @
--
-- = Combining Classes
--
-- Use 'cls' to combine multiple class strings, and 'clsWhen' for conditional classes:
--
-- @
-- cls ["p-4", "text-sm", clsWhen isActive "bg-blue-100"]
-- @
--
-- = Responsive Design
--
-- Tailwind's breakpoint prefixes are available as functions:
--
-- @
-- cls ["text-sm", md "text-base", lg "text-lg"]
-- -- Result: "text-sm md:text-base lg:text-lg"
-- @
--
-- = State Variants
--
-- Style elements based on user interaction:
--
-- @
-- cls ["bg-white", hover "bg-gray-100", focus "ring-2", active "bg-gray-200"]
-- @
module Lucid.Tailwind
  ( -- * Class Combinators
    cls,
    clsWhen,
    clsUnless,
    clsMaybe,

    -- * Breakpoint Prefixes
    -- $breakpoints
    sm,
    md,
    lg,
    xl,
    xxl,

    -- * State Variants
    -- $states
    hover,
    focus,
    active,
    disabled,

    -- * Group & Peer Variants
    -- $groupPeer
    group,
    groupHover,
    groupFocus,
    peer,
    peerChecked,
    peerFocus,
    peerDisabled,

    -- * Dark Mode
    dark,

    -- * Child Selectors
    -- $childSelectors
    first,
    last,
    odd,
    even,
    firstOfType,
    lastOfType,

    -- * Pseudo-Elements
    -- $pseudoElements
    before,
    after,
    placeholder,

    -- * Negative Values
    neg,

    -- * Grid Utilities
    -- $gridUtilities
    gridCols,
    responsiveGrid,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Prelude hiding (even, last, odd)

--------------------------------------------------------------------------------
-- Class Combinators

-- | Combine a list of class strings into a single space-separated string.
--
-- Empty strings are automatically filtered out, making it safe to use with
-- conditional combinators like 'clsWhen'.
--
-- ==== __Examples__
--
-- @
-- cls ["p-4", "text-sm", "bg-white"]
-- -- Result: "p-4 text-sm bg-white"
--
-- cls ["p-4", "", "bg-white"]
-- -- Result: "p-4 bg-white"
--
-- cls ["text-sm", md "text-base", lg "text-lg"]
-- -- Result: "text-sm md:text-base lg:text-lg"
-- @
cls :: [Text] -> Text
cls = Text.unwords . filter (not . Text.null)

-- | Include a class only when the condition is @True@.
--
-- Returns the class unchanged when the condition is @True@, or an empty
-- string when @False@. Designed to be used inside 'cls'.
--
-- ==== __Examples__
--
-- @
-- cls ["p-4", clsWhen isSelected "bg-blue-100", "rounded"]
-- -- When isSelected = True:  "p-4 bg-blue-100 rounded"
-- -- When isSelected = False: "p-4 rounded"
-- @
clsWhen :: Bool -> Text -> Text
clsWhen True c = c
clsWhen False _ = ""

-- | Include a class only when the condition is @False@.
--
-- The inverse of 'clsWhen'. Useful for default states.
--
-- ==== __Examples__
--
-- @
-- cls ["p-4", clsUnless isDisabled "cursor-pointer"]
-- -- When isDisabled = True:  "p-4"
-- -- When isDisabled = False: "p-4 cursor-pointer"
-- @
clsUnless :: Bool -> Text -> Text
clsUnless False c = c
clsUnless True _ = ""

-- | Include a class from a @Maybe@ value.
--
-- Returns the class if @Just@, or an empty string if @Nothing@.
-- Useful when a class is computed from optional data.
--
-- ==== __Examples__
--
-- @
-- cls ["p-4", clsMaybe (fmap colorClass mColor)]
-- -- When mColor = Just Red:  "p-4 text-red-500"
-- -- When mColor = Nothing:   "p-4"
-- @
clsMaybe :: Maybe Text -> Text
clsMaybe Nothing = ""
clsMaybe (Just c) = c

--------------------------------------------------------------------------------
-- Breakpoint Prefixes

-- $breakpoints
--
-- Tailwind uses a mobile-first breakpoint system. These functions prefix
-- classes with the appropriate breakpoint modifier.
--
-- | Breakpoint | Min Width | Prefix |
-- |------------|-----------|--------|
-- | sm         | 640px     | @sm:@  |
-- | md         | 768px     | @md:@  |
-- | lg         | 1024px    | @lg:@  |
-- | xl         | 1280px    | @xl:@  |
-- | 2xl        | 1536px    | @2xl:@ |
--
-- ==== __Examples__
--
-- @
-- -- Stack on mobile, row on tablet and up
-- cls ["flex flex-col", md "flex-row"]
--
-- -- Responsive text sizing
-- cls ["text-sm", md "text-base", lg "text-lg", xl "text-xl"]
--
-- -- Responsive padding
-- cls ["p-4", md "p-6", lg "p-8"]
-- @

-- | Prefix a class with @sm:@ breakpoint (640px+).
--
-- ==== __Examples__
--
-- @
-- sm "flex-row"     -- "sm:flex-row"
-- sm "text-base"    -- "sm:text-base"
-- sm "grid-cols-2"  -- "sm:grid-cols-2"
-- @
sm :: Text -> Text
sm c = "sm:" <> c

-- | Prefix a class with @md:@ breakpoint (768px+).
--
-- ==== __Examples__
--
-- @
-- md "flex-row"     -- "md:flex-row"
-- md "px-6"         -- "md:px-6"
-- md "hidden"       -- "md:hidden"
-- @
md :: Text -> Text
md c = "md:" <> c

-- | Prefix a class with @lg:@ breakpoint (1024px+).
--
-- ==== __Examples__
--
-- @
-- lg "grid-cols-3"  -- "lg:grid-cols-3"
-- lg "text-lg"      -- "lg:text-lg"
-- @
lg :: Text -> Text
lg c = "lg:" <> c

-- | Prefix a class with @xl:@ breakpoint (1280px+).
--
-- ==== __Examples__
--
-- @
-- xl "max-w-6xl"    -- "xl:max-w-6xl"
-- xl "px-8"         -- "xl:px-8"
-- @
xl :: Text -> Text
xl c = "xl:" <> c

-- | Prefix a class with @2xl:@ breakpoint (1536px+).
--
-- ==== __Examples__
--
-- @
-- xxl "max-w-7xl"   -- "2xl:max-w-7xl"
-- xxl "text-2xl"    -- "2xl:text-2xl"
-- @
xxl :: Text -> Text
xxl c = "2xl:" <> c

--------------------------------------------------------------------------------
-- State Variants

-- $states
--
-- State variants style elements based on user interaction or element state.
--
-- ==== __Examples__
--
-- @
-- -- Interactive button
-- cls
--   [ "bg-blue-500 text-white px-4 py-2"
--   , hover "bg-blue-600"
--   , focus "outline-none ring-2 ring-blue-300"
--   , active "bg-blue-700"
--   , disabled "opacity-50 cursor-not-allowed"
--   ]
-- @

-- | Style when the user hovers over the element.
--
-- ==== __Examples__
--
-- @
-- hover "bg-gray-100"    -- "hover:bg-gray-100"
-- hover "underline"      -- "hover:underline"
-- hover "scale-105"      -- "hover:scale-105"
-- @
hover :: Text -> Text
hover c = "hover:" <> c

-- | Style when the element has focus.
--
-- ==== __Examples__
--
-- @
-- focus "outline-none"   -- "focus:outline-none"
-- focus "ring-2"         -- "focus:ring-2"
-- focus "border-blue-500" -- "focus:border-blue-500"
-- @
focus :: Text -> Text
focus c = "focus:" <> c

-- | Style when the element is being actively pressed.
--
-- ==== __Examples__
--
-- @
-- active "bg-gray-200"   -- "active:bg-gray-200"
-- active "scale-95"      -- "active:scale-95"
-- @
active :: Text -> Text
active c = "active:" <> c

-- | Style when the element is disabled.
--
-- ==== __Examples__
--
-- @
-- disabled "opacity-50"          -- "disabled:opacity-50"
-- disabled "cursor-not-allowed"  -- "disabled:cursor-not-allowed"
-- @
disabled :: Text -> Text
disabled c = "disabled:" <> c

--------------------------------------------------------------------------------
-- Group & Peer Variants

-- $groupPeer
--
-- Group and peer variants allow styling elements based on the state of a
-- parent or sibling element.
--
-- ==== __Group Example__
--
-- Style children when hovering over a parent marked with @class="group"@:
--
-- @
-- div_ [class_ "group p-4 hover:bg-gray-100"] $ do
--   span_ [class_ $ cls ["text-gray-600", groupHover "text-gray-900"]] "Label"
--   span_ [class_ $ cls ["text-gray-400", groupHover "text-gray-600"]] "→"
-- @
--
-- ==== __Peer Example__
--
-- Style an element based on a preceding sibling marked with @class="peer"@:
--
-- @
-- input_ [class_ "peer", type_ "checkbox"]
-- label_ [class_ $ cls ["text-gray-600", peerChecked "text-blue-600"]] "Option"
-- @

-- | Mark an element as a group parent.
--
-- Add this to a parent element, then use 'groupHover' etc. on children.
--
-- ==== __Examples__
--
-- @
-- div_ [class_ $ cls [group, "p-4"]] $ do
--   span_ [class_ $ groupHover "text-blue-500"] "Hover parent to change me"
-- @
group :: Text
group = "group"

-- | Style when a parent @group@ is hovered.
--
-- ==== __Examples__
--
-- @
-- groupHover "text-blue-500"   -- "group-hover:text-blue-500"
-- groupHover "visible"         -- "group-hover:visible"
-- @
groupHover :: Text -> Text
groupHover c = "group-hover:" <> c

-- | Style when a parent @group@ has focus.
--
-- ==== __Examples__
--
-- @
-- groupFocus "ring-2"  -- "group-focus:ring-2"
-- @
groupFocus :: Text -> Text
groupFocus c = "group-focus:" <> c

-- | Mark an element as a peer for sibling-based styling.
--
-- Add this to an element, then use 'peerChecked' etc. on following siblings.
--
-- ==== __Examples__
--
-- @
-- input_ [class_ peer, type_ "checkbox", id_ "toggle"]
-- label_ [class_ $ peerChecked "font-bold"] "Label styled by checkbox state"
-- @
peer :: Text
peer = "peer"

-- | Style when a preceding @peer@ checkbox/radio is checked.
--
-- ==== __Examples__
--
-- @
-- peerChecked "bg-blue-100"   -- "peer-checked:bg-blue-100"
-- peerChecked "line-through"  -- "peer-checked:line-through"
-- @
peerChecked :: Text -> Text
peerChecked c = "peer-checked:" <> c

-- | Style when a preceding @peer@ element has focus.
--
-- ==== __Examples__
--
-- @
-- peerFocus "text-blue-500"  -- "peer-focus:text-blue-500"
-- @
peerFocus :: Text -> Text
peerFocus c = "peer-focus:" <> c

-- | Style when a preceding @peer@ element is disabled.
--
-- ==== __Examples__
--
-- @
-- peerDisabled "opacity-50"  -- "peer-disabled:opacity-50"
-- @
peerDisabled :: Text -> Text
peerDisabled c = "peer-disabled:" <> c

--------------------------------------------------------------------------------
-- Dark Mode

-- | Style for dark mode.
--
-- Tailwind's dark mode can be configured as @media@ (system preference) or
-- @class@ (manual toggle). This prefix works with both.
--
-- ==== __Examples__
--
-- @
-- cls ["bg-white", dark "bg-gray-900"]
-- -- Light mode: white background
-- -- Dark mode: dark gray background
--
-- cls ["text-gray-900", dark "text-gray-100"]
-- -- Inverted text colors for dark mode
-- @
dark :: Text -> Text
dark c = "dark:" <> c

--------------------------------------------------------------------------------
-- Child Selectors

-- $childSelectors
--
-- Style elements based on their position among siblings.
--
-- ==== __Examples__
--
-- @
-- -- Remove top border from first item in a list
-- li_ [class_ $ cls ["border-t", first "border-t-0"]] content
--
-- -- Alternate row colors
-- tr_ [class_ $ cls ["bg-white", odd "bg-gray-50"]] content
-- @

-- | Style the first child element.
--
-- ==== __Examples__
--
-- @
-- first "mt-0"          -- "first:mt-0"
-- first "rounded-t-lg"  -- "first:rounded-t-lg"
-- @
first :: Text -> Text
first c = "first:" <> c

-- | Style the last child element.
--
-- ==== __Examples__
--
-- @
-- last "mb-0"           -- "last:mb-0"
-- last "rounded-b-lg"   -- "last:rounded-b-lg"
-- last "border-b-0"     -- "last:border-b-0"
-- @
last :: Text -> Text
last c = "last:" <> c

-- | Style odd-numbered children (1st, 3rd, 5th...).
--
-- ==== __Examples__
--
-- @
-- odd "bg-gray-50"  -- "odd:bg-gray-50"
-- @
odd :: Text -> Text
odd c = "odd:" <> c

-- | Style even-numbered children (2nd, 4th, 6th...).
--
-- ==== __Examples__
--
-- @
-- even "bg-gray-100"  -- "even:bg-gray-100"
-- @
even :: Text -> Text
even c = "even:" <> c

-- | Style the first element of its type among siblings.
--
-- ==== __Examples__
--
-- @
-- firstOfType "mt-0"  -- "first-of-type:mt-0"
-- @
firstOfType :: Text -> Text
firstOfType c = "first-of-type:" <> c

-- | Style the last element of its type among siblings.
--
-- ==== __Examples__
--
-- @
-- lastOfType "mb-0"  -- "last-of-type:mb-0"
-- @
lastOfType :: Text -> Text
lastOfType c = "last-of-type:" <> c

--------------------------------------------------------------------------------
-- Pseudo-Elements

-- $pseudoElements
--
-- Style pseudo-elements like @::before@, @::after@, and @::placeholder@.
--
-- Note: @before@ and @after@ require @content-['']@ to be visible.
--
-- ==== __Examples__
--
-- @
-- -- Add a decorative element
-- div_ [class_ $ cls [before "content-['*']", before "text-red-500"]] "Required"
--
-- -- Style placeholder text
-- input_ [class_ $ placeholder "text-gray-400", placeholder_ "Enter text..."]
-- @

-- | Style the @::before@ pseudo-element.
--
-- Remember to set content, e.g., @before "content-['']"@.
--
-- ==== __Examples__
--
-- @
-- before "content-['→']"   -- "before:content-['→']"
-- before "absolute"        -- "before:absolute"
-- before "bg-blue-500"     -- "before:bg-blue-500"
-- @
before :: Text -> Text
before c = "before:" <> c

-- | Style the @::after@ pseudo-element.
--
-- Remember to set content, e.g., @after "content-['']"@.
--
-- ==== __Examples__
--
-- @
-- after "content-['']"    -- "after:content-['']"
-- after "absolute"        -- "after:absolute"
-- after "w-full h-0.5"    -- "after:w-full h-0.5"
-- @
after :: Text -> Text
after c = "after:" <> c

-- | Style the @::placeholder@ pseudo-element for inputs.
--
-- ==== __Examples__
--
-- @
-- placeholder "text-gray-400"   -- "placeholder:text-gray-400"
-- placeholder "italic"          -- "placeholder:italic"
-- @
placeholder :: Text -> Text
placeholder c = "placeholder:" <> c

--------------------------------------------------------------------------------
-- Negative Values

-- | Prefix a class with @-@ for negative values.
--
-- Tailwind uses a leading dash for negative spacing, positioning, etc.
--
-- ==== __Examples__
--
-- @
-- neg "mt-4"      -- "-mt-4" (negative margin-top)
-- neg "translate-x-1" -- "-translate-x-1"
-- neg "rotate-45" -- "-rotate-45"
-- @
neg :: Text -> Text
neg c = "-" <> c

--------------------------------------------------------------------------------
-- Grid Utilities

-- $gridUtilities
--
-- Helper functions for generating Tailwind grid classes dynamically.
--
-- ==== __Examples__
--
-- @
-- -- Dynamic column count
-- div_ [class_ $ cls ["grid", gridCols 3, "gap-4"]] content
--
-- -- Responsive grid: 1 col on mobile, 2 on tablet, 3 on desktop
-- div_ [class_ $ cls [responsiveGrid 1 2 3, "gap-4"]] content
-- @

-- | Generate a @grid-cols-N@ class.
--
-- ==== __Examples__
--
-- @
-- gridCols 1   -- "grid-cols-1"
-- gridCols 2   -- "grid-cols-2"
-- gridCols 3   -- "grid-cols-3"
-- gridCols 12  -- "grid-cols-12"
-- @
gridCols :: Int -> Text
gridCols n = "grid-cols-" <> Text.pack (show n)

-- | Generate a responsive grid class string.
--
-- Creates a grid that changes column count at different breakpoints.
-- Includes the @grid@ class automatically.
--
-- ==== __Examples__
--
-- @
-- responsiveGrid 1 2 3
-- -- "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3"
--
-- responsiveGrid 1 2 4
-- -- "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4"
--
-- -- Use in a template:
-- div_ [class_ $ cls [responsiveGrid 1 2 3, "gap-4"]] $ do
--   div_ "Item 1"
--   div_ "Item 2"
--   div_ "Item 3"
-- @
responsiveGrid :: Int -> Int -> Int -> Text
responsiveGrid mobileCols tabletCols desktopCols =
  cls
    [ "grid",
      gridCols mobileCols,
      md (gridCols tabletCols),
      lg (gridCols desktopCols)
    ]
