-- | Cards: a box with a background and padding that holds a part of a page.
--
-- Each variant copies a look that three or more pages had before this
-- module. A caller adds classes for the parts that differ by page, such as a
-- bottom margin.
--
-- > import Component.Card qualified as Card
-- >
-- > Card.card Card.Detail [Tokens.mb6] $ do
-- >   Lucid.h3_ "Details"
-- >   ...
--
-- 'card' renders a @div@ and 'section' renders a @section@. For another
-- element, or for more attributes, use 'classes' in the @class@ attribute.
module Component.Card
  ( -- * Variants
    Variant (..),

    -- * Rendering
    card,
    section,
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

-- | The look of a card.
data Variant
  = -- | The message of an empty dashboard list: the alternate background, a
    -- muted border, large padding, and centered text.
    EmptyState
  | -- | The header of a dashboard form or detail page: the page background,
    -- full width, and a bottom margin.
    Header
  | -- | The box around a login, register, or password form.
    Auth
  | -- | A panel of a dashboard detail page: the page background and rounded
    -- corners.
    Detail
  | -- | A box inside a panel: the alternate background and a muted border.
    Inset
  | -- | The box around the table of a dashboard list page: the page
    -- background and rounded corners. It clips the table to the corners.
    TablePanel
  deriving stock (Eq, Show, Enum, Bounded)

--------------------------------------------------------------------------------
-- Classes

-- | The classes of a card with this variant.
classes :: Variant -> [Text]
classes = \case
  EmptyState -> [Tokens.bgAlt, Tokens.border2, Tokens.borderMuted, "p-12", "text-center"]
  Header -> [Tokens.bgMain, Tokens.fgPrimary, Tokens.p6, Tokens.mb8, Tokens.fullWidth]
  Auth -> [Tokens.bgMain, Tokens.p8]
  Detail -> [Tokens.bgMain, "rounded", Tokens.p6]
  Inset -> [Tokens.bgAlt, Tokens.border2, Tokens.borderMuted, Tokens.p4]
  TablePanel -> [Tokens.bgMain, "rounded", "overflow-hidden"]

--------------------------------------------------------------------------------
-- Rendering

-- | A card as a @div@.
card ::
  Variant ->
  -- | More classes, after the classes of the variant
  [Text] ->
  Lucid.Html () ->
  Lucid.Html ()
card variant extra = Lucid.div_ [class_ $ base (classes variant <> extra)]

-- | A card as a @section@.
section ::
  Variant ->
  -- | More classes, after the classes of the variant
  [Text] ->
  Lucid.Html () ->
  Lucid.Html ()
section variant extra = Lucid.section_ [class_ $ base (classes variant <> extra)]

--------------------------------------------------------------------------------
-- Tailwind

-- | Every class that this module can render. "Component.Tailwind" puts them in
-- the safelist, because a dashboard page can first appear in an HTMX swap.
safelist :: [Text]
safelist = nub $ foldMap classes [minBound .. maxBound]
