{-# LANGUAGE QuasiQuotes #-}

-- | The boxed form controls: the single-line input, the textarea, and the
-- select.
--
-- The form builder and the checkout page draw their controls with this
-- module. Each control copies a rule that @forms.css@ had, so the public
-- forms keep their look. Put a control in a "Component.Field" wrapper with a
-- label.
--
-- > import Component.Input qualified as Input
-- >
-- > Input.input [Lucid.type_ "text", Lucid.name_ "city", Input.errorWhen "showErrors && !cityValid"]
-- > Input.select [Lucid.name_ "state"] stateOptions
--
-- A control puts its own @class@ attribute first. A caller gives the other
-- attributes, such as @type@, @name@, and the Alpine attributes, and gives no
-- @class@ attribute.
module Component.Input
  ( -- * Rendering
    input,
    textarea,
    select,

    -- * Error state
    errorWhen,
    errorClass,

    -- * Tailwind
    safelist,
  )
where

--------------------------------------------------------------------------------

import Data.List (nub)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Lucid qualified
import Lucid.Alpine (xBindClass_)

--------------------------------------------------------------------------------
-- Classes

-- | The classes that every boxed control has.
--
-- The font stack, the timing function, and the outline use arbitrary values.
-- The @font-mono@ stack of the Tailwind config has more fonts, the Tailwind
-- @ease-in-out@ is a cubic Bezier curve, and @outline-none@ sets a
-- transparent outline in place of no outline.
boxClasses :: [Text]
boxClasses =
  [ "w-full",
    "p-3",
    "border",
    "border-[var(--theme-border)]",
    "bg-[var(--theme-bg)]",
    "text-[var(--theme-fg)]",
    "[font-family:ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace]",
    "transition-[border-color]",
    "duration-150",
    "ease-[ease-in-out]",
    "focus:border-[var(--theme-info)]",
    "focus:[outline:none]"
  ]

-- | A disabled input has a muted look.
inputClasses :: [Text]
inputClasses =
  boxClasses
    <> [ "disabled:bg-[var(--theme-bg-alt)]",
         "disabled:text-[var(--theme-fg-muted)]",
         "disabled:cursor-not-allowed"
       ]

textareaClasses :: [Text]
textareaClasses = boxClasses <> ["leading-relaxed", "resize-y"]

-- | The Tailwind base rule @:disabled { cursor: default }@ came after the old
-- rule and won over its pointer. @disabled:cursor-default@ keeps that look.
selectClasses :: [Text]
selectClasses = boxClasses <> ["cursor-pointer", "disabled:cursor-default"]

--------------------------------------------------------------------------------
-- Rendering

-- | A single-line input. Give its @type@ in the attributes.
input :: [Lucid.Attributes] -> Lucid.Html ()
input attrs = Lucid.input_ (class_ (base inputClasses) : attrs)

-- | A multi-line text input with its initial text.
textarea :: [Lucid.Attributes] -> Lucid.Html () -> Lucid.Html ()
textarea attrs = Lucid.textarea_ (class_ (base textareaClasses) : attrs)

-- | A select with its options.
select :: [Lucid.Attributes] -> Lucid.Html () -> Lucid.Html ()
select attrs = Lucid.select_ (class_ (base selectClasses) : attrs)

--------------------------------------------------------------------------------
-- Error state

-- | The class that gives a control the error border.
--
-- It is important, so it wins over the focus border, the same as the old
-- error rules.
errorClass :: Text
errorClass = "!border-[var(--theme-error)]"

-- | An Alpine attribute that adds 'errorClass' when the condition is true.
errorWhen ::
  -- | Alpine condition that is true when the value is not valid
  Text ->
  Lucid.Attributes
errorWhen condition = xBindClass_ [i|{ '#{errorClass}': #{condition} }|]

--------------------------------------------------------------------------------
-- Tailwind

-- | Every class that this module can render. "Component.Tailwind" puts them in
-- the safelist. Alpine adds 'errorClass' only after a failed validation, so
-- the Tailwind CDN must write its rule before it appears.
safelist :: [Text]
safelist = nub $ inputClasses <> textareaClasses <> selectClasses <> [errorClass]
