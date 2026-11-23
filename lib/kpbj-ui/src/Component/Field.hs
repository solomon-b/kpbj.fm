-- | The parts of a form field around its control: the wrapper, the labels,
-- the hint, and the error message.
--
-- The form builder and the checkout page draw their fields with these parts.
-- Each part copies a rule that @forms.css@ had, so the public forms keep
-- their look.
--
-- > import Component.Field qualified as Field
-- > import Component.Input qualified as Input
-- >
-- > Field.field [] $ do
-- >   Field.label Field.Enabled [Lucid.for_ "email"] "Email"
-- >   Input.input [Lucid.type_ "email", Lucid.id_ "email", Lucid.name_ "email"]
-- >   Field.hint "We send the receipt here."
--
-- A part puts its own @class@ attribute first. A caller gives the other
-- attributes, and gives no @class@ attribute.
module Component.Field
  ( -- * State
    State (..),

    -- * Rendering
    field,
    label,
    groupLabel,
    hint,
    errorMessage,

    -- * Tailwind
    safelist,
  )
where

--------------------------------------------------------------------------------

import Data.List (nub)
import Data.Text (Text)
import Design (base, class_)
import Lucid qualified

--------------------------------------------------------------------------------
-- State

-- | The state of the control that a label names. A label of a disabled
-- control has the muted color.
data State
  = Enabled
  | Disabled
  deriving stock (Eq, Show, Enum, Bounded)

--------------------------------------------------------------------------------
-- Classes

-- | A field has a bottom margin, except the last field of its parent.
fieldClasses :: [Text]
fieldClasses = ["mb-4", "last:mb-0"]

labelClasses :: State -> [Text]
labelClasses state = ["block", "font-bold", "mb-2", color]
  where
    color = case state of
      Enabled -> "text-[var(--theme-fg)]"
      Disabled -> "text-[var(--theme-fg-muted)]"

-- | The font sizes use arbitrary values. @text-xs@ and @text-sm@ also set the
-- line height, and the old rules did not.
hintClasses :: [Text]
hintClasses = ["text-[0.75rem]", "text-[var(--theme-fg-muted)]", "mt-1"]

errorClasses :: [Text]
errorClasses = ["block", "mt-1", "text-[0.875rem]", "text-[var(--theme-error)]"]

--------------------------------------------------------------------------------
-- Rendering

-- | The wrapper of one field. It holds the label, the control, the error
-- message, and the hint.
field :: [Lucid.Attributes] -> Lucid.Html () -> Lucid.Html ()
field attrs = Lucid.div_ (class_ (base fieldClasses) : attrs)

-- | The label of one control. Give the id of the control with 'Lucid.for_'.
label :: State -> [Lucid.Attributes] -> Lucid.Html () -> Lucid.Html ()
label state attrs = Lucid.label_ (class_ (base (labelClasses state)) : attrs)

-- | The label of a group of controls, such as a set of radio buttons. It is a
-- paragraph, because a @label@ element names one control.
groupLabel :: Lucid.Html () -> Lucid.Html ()
groupLabel = Lucid.p_ [class_ $ base (labelClasses Enabled)]

-- | A line of help text below a control.
hint :: Lucid.Html () -> Lucid.Html ()
hint = Lucid.p_ [class_ $ base hintClasses]

-- | An error message below a control.
--
-- The message starts hidden. Give it an Alpine @x-show@ attribute that shows
-- it when the value is not valid.
errorMessage :: [Lucid.Attributes] -> Lucid.Html () -> Lucid.Html ()
errorMessage attrs =
  Lucid.span_ ([class_ $ base errorClasses, Lucid.style_ "display: none;"] <> attrs)

--------------------------------------------------------------------------------
-- Tailwind

-- | Every class that this module can render. "Component.Tailwind" puts them in
-- the safelist, because a form can first appear in an HTMX swap.
safelist :: [Text]
safelist =
  nub $
    fieldClasses
      <> foldMap labelClasses [minBound .. maxBound]
      <> hintClasses
      <> errorClasses
