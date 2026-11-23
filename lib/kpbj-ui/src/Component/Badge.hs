-- | Status badges: a short label in a tinted box, such as @Active@ or
-- @Draft@.
--
-- A badge has a shape and a tone. The shape sets the size and the weight.
-- The tone sets the colors.
--
-- > import Component.Badge qualified as Badge
-- >
-- > Badge.badge Badge.Regular Badge.Success "Published"
--
-- Each shape copies a look that the pages had before this module. The tints
-- of 'Success', 'Warning', 'Error', 'Info', and 'Muted' use an opacity
-- modifier on a CSS variable color. Tailwind writes no rule for those, so
-- these badges show colored text with no fill.
module Component.Badge
  ( -- * Variants
    Shape (..),
    Tone (..),

    -- * Rendering
    badge,
    classes,

    -- * Tailwind
    safelist,
  )
where

--------------------------------------------------------------------------------

import Data.List (nub)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified

--------------------------------------------------------------------------------
-- Variants

-- | The size and the weight of a badge.
data Shape
  = -- | Small text, bold. Most status badges in the dashboard tables use it.
    Regular
  | -- | Extra small text, bold, less side padding. The blog post and episode
    -- badges use it.
    Compact
  | -- | Extra small text, normal weight, more vertical padding. The playout
    -- source type badge uses it.
    Tall
  deriving stock (Eq, Show, Enum, Bounded)

-- | The colors of a badge.
data Tone
  = Success
  | Warning
  | Error
  | Info
  | -- | The alternate background with the muted text.
    Neutral
  | -- | The alternate background with the main text.
    Plain
  | -- | A muted tint with the muted text.
    Muted
  | -- | The inverse background with the inverse text.
    Inverse
  deriving stock (Eq, Show, Enum, Bounded)

--------------------------------------------------------------------------------
-- Classes

shapeClasses :: Shape -> [Text]
shapeClasses = \case
  Regular -> ["inline-block", Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, "rounded"]
  Compact -> ["inline-block", "px-2", "py-1", "rounded", Tokens.textXs, Tokens.fontBold]
  Tall -> [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded"]

toneClasses :: Tone -> [Text]
toneClasses = \case
  Success -> [Tokens.successBg, Tokens.successText]
  Warning -> [Tokens.warningBg, Tokens.warningText]
  Error -> [Tokens.errorBg, Tokens.errorText]
  Info -> [Tokens.infoBg, Tokens.infoText]
  Neutral -> [Tokens.bgAlt, Tokens.fgMuted]
  Plain -> [Tokens.bgAlt, Tokens.fgPrimary]
  Muted -> [Tokens.mutedBg, Tokens.fgMuted]
  Inverse -> [Tokens.bgInverse, Tokens.fgInverse]

-- | The classes of a badge with this shape and tone.
classes :: Shape -> Tone -> [Text]
classes shape tone = shapeClasses shape <> toneClasses tone

--------------------------------------------------------------------------------
-- Rendering

-- | A badge as a @span@ with the label inside.
badge :: Shape -> Tone -> Lucid.Html () -> Lucid.Html ()
badge shape tone = Lucid.span_ [class_ $ base (classes shape tone)]

--------------------------------------------------------------------------------
-- Tailwind

-- | Every class that this module can render. "Component.Tailwind" puts them in
-- the safelist, because a dashboard table row can first appear in an HTMX
-- swap.
safelist :: [Text]
safelist =
  nub $
    foldMap shapeClasses [minBound .. maxBound]
      <> foldMap toneClasses [minBound .. maxBound]
