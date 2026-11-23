{-# LANGUAGE QuasiQuotes #-}

-- | The choice controls: the checkbox and the radio buttons.
--
-- The form builder draws its checkbox and radio fields with this module.
-- Each part copies a rule that @forms.css@ had. Put a checkbox or a radio
-- group in a "Component.Field" wrapper.
--
-- > import Component.Choice qualified as Choice
-- >
-- > Choice.checkbox "agree" [Lucid.name_ "agree"] "I agree" Choice.Required Nothing
-- >
-- > Choice.radioGroup [Choice.radioErrorWhen "showErrors && !sizeValid"] $ do
-- >   Choice.radioOption "size-s" [Lucid.name_ "size", Lucid.value_ "s"] "Small" Nothing
-- >   Choice.radioOption "size-l" [Lucid.name_ "size", Lucid.value_ "l"] "Large" (Just "For two")
--
-- A part puts its own @class@ attribute first. A caller gives the other
-- attributes, and gives no @class@ attribute.
module Component.Choice
  ( -- * Checkbox
    Requirement (..),
    checkbox,

    -- * Radio buttons
    radioGroup,
    radioErrorWhen,
    radioOption,

    -- * Tailwind
    safelist,
  )
where

--------------------------------------------------------------------------------

import Control.Monad (forM_, when)
import Data.List (nub)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Design (base, class_)
import Lucid qualified
import Lucid.Alpine (xBindClass_)

--------------------------------------------------------------------------------
-- Classes

-- | The classes of a checkbox or a radio input.
--
-- The Tailwind base rule @:disabled { cursor: default }@ came after the old
-- rules and won over their pointer. @disabled:cursor-default@ keeps that look.
markClasses :: [Text]
markClasses = ["mt-1", "cursor-pointer", "disabled:cursor-default"]

checkboxRowClasses :: [Text]
checkboxRowClasses = ["flex", "items-start", "gap-2"]

checkboxLabelClasses :: [Text]
checkboxLabelClasses = ["cursor-pointer", "text-[var(--theme-fg)]"]

-- | The text of a checkbox or a radio option.
optionTextClasses :: [Text]
optionTextClasses = ["font-medium"]

requiredMarkClasses :: [Text]
requiredMarkClasses = ["text-[var(--theme-error)]", "ml-1"]

checkboxDescriptionClasses :: [Text]
checkboxDescriptionClasses = ["text-[0.875rem]", "text-[var(--theme-fg-muted)]", "mt-1"]

radioGroupClasses :: [Text]
radioGroupClasses = ["flex", "flex-col", "gap-2"]

-- | The error look of a radio group. A negative margin keeps the group in
-- place when the padding and the border appear.
radioGroupErrorClasses :: [Text]
radioGroupErrorClasses = ["p-2", "-m-2", "border", "border-[var(--theme-error)]"]

radioOptionClasses :: [Text]
radioOptionClasses = ["flex", "items-start", "gap-2", "cursor-pointer", "text-[var(--theme-fg)]"]

radioDescriptionClasses :: [Text]
radioDescriptionClasses = ["text-[0.875rem]", "text-[var(--theme-fg-muted)]", "ml-6"]

--------------------------------------------------------------------------------
-- Checkbox

-- | Whether a checkbox must be checked. A required checkbox shows a mark
-- after its label.
data Requirement
  = Optional
  | Required
  deriving stock (Eq, Show, Enum, Bounded)

-- | A checkbox with its label and an optional description below the label.
-- A click on the label or the description changes the checkbox.
checkbox ::
  -- | Id of the input
  Text ->
  -- | More attributes of the input, such as @name@, @checked@, and the Alpine
  -- attributes
  [Lucid.Attributes] ->
  -- | Label
  Lucid.Html () ->
  Requirement ->
  -- | Description
  Maybe (Lucid.Html ()) ->
  Lucid.Html ()
checkbox inputId attrs labelHtml requirement mDescription =
  Lucid.div_ [class_ $ base checkboxRowClasses] $ do
    Lucid.input_ ([Lucid.type_ "checkbox", Lucid.id_ inputId, class_ $ base markClasses] <> attrs)
    Lucid.label_ [Lucid.for_ inputId, class_ $ base checkboxLabelClasses] $ do
      Lucid.span_ [class_ $ base optionTextClasses] $ do
        labelHtml
        when (requirement == Required) $
          Lucid.span_ [class_ $ base requiredMarkClasses] "*"
      forM_ mDescription $
        Lucid.div_ [class_ $ base checkboxDescriptionClasses]

--------------------------------------------------------------------------------
-- Radio buttons

-- | The box that holds the options of one radio group.
radioGroup :: [Lucid.Attributes] -> Lucid.Html () -> Lucid.Html ()
radioGroup attrs = Lucid.div_ (class_ (base radioGroupClasses) : attrs)

-- | An Alpine attribute for 'radioGroup' that adds the error look when the
-- condition is true.
radioErrorWhen ::
  -- | Alpine condition that is true when no valid option is selected
  Text ->
  Lucid.Attributes
radioErrorWhen condition =
  xBindClass_ [i|{ '#{Text.unwords radioGroupErrorClasses}': #{condition} }|]

-- | One radio option with its label and an optional description. The whole
-- row is the label of the input.
radioOption ::
  -- | Id of the input
  Text ->
  -- | More attributes of the input, such as @name@, @value@, and the Alpine
  -- attributes
  [Lucid.Attributes] ->
  -- | Label
  Lucid.Html () ->
  -- | Description
  Maybe (Lucid.Html ()) ->
  Lucid.Html ()
radioOption inputId attrs labelHtml mDescription =
  Lucid.label_ [Lucid.for_ inputId, class_ $ base radioOptionClasses] $ do
    Lucid.input_ ([Lucid.type_ "radio", Lucid.id_ inputId, class_ $ base markClasses] <> attrs)
    Lucid.span_ [class_ $ base optionTextClasses] labelHtml
    forM_ mDescription $
      Lucid.span_ [class_ $ base radioDescriptionClasses]

--------------------------------------------------------------------------------
-- Tailwind

-- | Every class that this module can render. "Component.Tailwind" puts them in
-- the safelist. Alpine adds the error look of a radio group only after a
-- failed validation, so the Tailwind CDN must write its rules before it
-- appears.
safelist :: [Text]
safelist =
  nub $
    markClasses
      <> checkboxRowClasses
      <> checkboxLabelClasses
      <> optionTextClasses
      <> requiredMarkClasses
      <> checkboxDescriptionClasses
      <> radioGroupClasses
      <> radioGroupErrorClasses
      <> radioOptionClasses
      <> radioDescriptionClasses
