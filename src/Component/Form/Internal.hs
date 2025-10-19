{-# LANGUAGE QuasiQuotes #-}

module Component.Form.Internal
  ( -- * Low-level form field builders
    textInput,
    textInputWithValidation,
    textarea,
    textareaWithValidation,
    select,
    numberInput,
    checkbox,
    fileInput,
    hiddenInput,

    -- * Types
    SelectOption (..),

    -- * Helpers
    capitalizeFirst,
  )
where

--------------------------------------------------------------------------------

import Data.Char qualified
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, display)
import Lucid qualified
import Lucid.Base (Attributes)
import Lucid.Extras (xBindClass_, xModel_, xOnBlur_, xOnChange_, xOnInput_)

--------------------------------------------------------------------------------
-- Attribute Helpers

-- | Create an optional attribute from a Maybe value
optionalAttr :: (Text -> Attributes) -> Maybe Text -> [Attributes]
optionalAttr attrFn = maybe [] (\v -> [attrFn v])

-- | Create an optional attribute from a Maybe value with a transformation
optionalAttrWith :: (Display a) => (Text -> Attributes) -> Maybe a -> [Attributes]
optionalAttrWith attrFn = maybe [] (\v -> [attrFn (display v)])

-- | Create a boolean attribute (present or absent)
boolAttr :: Attributes -> Bool -> [Attributes]
boolAttr attr True = [attr]
boolAttr _ False = []

--------------------------------------------------------------------------------
-- Text input fields

-- | Simple text input without Alpine.js validation
textInput ::
  -- | name
  Text ->
  -- | id
  Maybe Text ->
  -- | value
  Maybe Text ->
  -- | placeholder
  Maybe Text ->
  -- | required
  Bool ->
  -- | minLength
  Maybe Int ->
  -- | maxLength
  Maybe Int ->
  -- | pattern
  Maybe Text ->
  -- | title
  Maybe Text ->
  -- | classes
  Text ->
  -- | disabled
  Bool ->
  Lucid.Html ()
textInput name mId mValue mPlaceholder required mMinLen mMaxLen mPattern mTitle classes disabled =
  Lucid.input_ $
    [ Lucid.type_ "text",
      Lucid.name_ name,
      Lucid.class_ classes
    ]
      <> optionalAttr Lucid.id_ mId
      <> optionalAttr Lucid.value_ mValue
      <> optionalAttr Lucid.placeholder_ mPlaceholder
      <> boolAttr (Lucid.required_ "") required
      <> optionalAttrWith Lucid.minlength_ mMinLen
      <> optionalAttrWith Lucid.maxlength_ mMaxLen
      <> optionalAttr Lucid.pattern_ mPattern
      <> optionalAttr Lucid.title_ mTitle
      <> boolAttr (Lucid.disabled_ "") disabled

-- | Text input with Alpine.js validation binding
textInputWithValidation ::
  -- | fieldName (for Alpine binding)
  Text ->
  -- | name
  Text ->
  -- | id
  Maybe Text ->
  -- | value
  Maybe Text ->
  -- | placeholder
  Maybe Text ->
  -- | required
  Bool ->
  -- | minLength
  Maybe Int ->
  -- | maxLength
  Maybe Int ->
  -- | pattern
  Maybe Text ->
  -- | title
  Maybe Text ->
  -- | classes (normal state)
  Text ->
  -- | error classes (invalid state)
  Text ->
  -- | disabled
  Bool ->
  Lucid.Html ()
textInputWithValidation fieldName name mId mValue mPlaceholder required mMinLen mMaxLen mPattern mTitle classes errorClasses disabled =
  Lucid.input_ $
    [ Lucid.type_ "text",
      Lucid.name_ name,
      xModel_ [i|fields.#{fieldName}.value|],
      xBindClass_ [i|showErrors && !fields.#{fieldName}.isValid ? '#{errorClasses}' : '#{classes}'|],
      xOnInput_ [i|showErrors && validate#{capitalizeFirst fieldName}()|],
      xOnBlur_ [i|showErrors && validate#{capitalizeFirst fieldName}()|],
      Lucid.class_ classes
    ]
      <> optionalAttr Lucid.id_ mId
      <> optionalAttr Lucid.value_ mValue
      <> optionalAttr Lucid.placeholder_ mPlaceholder
      <> boolAttr (Lucid.required_ "") required
      <> optionalAttrWith Lucid.minlength_ mMinLen
      <> optionalAttrWith Lucid.maxlength_ mMaxLen
      <> optionalAttr Lucid.pattern_ mPattern
      <> optionalAttr Lucid.title_ mTitle
      <> boolAttr (Lucid.disabled_ "") disabled

--------------------------------------------------------------------------------
-- Textarea fields

-- | Simple textarea without Alpine.js validation
textarea ::
  -- | name
  Text ->
  -- | id
  Maybe Text ->
  -- | value
  Maybe Text ->
  -- | placeholder
  Maybe Text ->
  -- | required
  Bool ->
  -- | rows
  Int ->
  -- | maxLength
  Maybe Int ->
  -- | classes
  Text ->
  -- | disabled
  Bool ->
  Lucid.Html ()
textarea name mId mValue mPlaceholder required rows mMaxLen classes disabled =
  Lucid.textarea_
    ( [ Lucid.name_ name,
        Lucid.rows_ (display rows),
        Lucid.class_ classes
      ]
        <> optionalAttr Lucid.id_ mId
        <> optionalAttr Lucid.placeholder_ mPlaceholder
        <> boolAttr (Lucid.required_ "") required
        <> optionalAttrWith Lucid.maxlength_ mMaxLen
        <> boolAttr (Lucid.disabled_ "") disabled
    )
    (maybe mempty Lucid.toHtml mValue)

-- | Textarea with Alpine.js validation binding
textareaWithValidation ::
  -- | fieldName (for Alpine binding)
  Text ->
  -- | name
  Text ->
  -- | id
  Maybe Text ->
  -- | value
  Maybe Text ->
  -- | placeholder
  Maybe Text ->
  -- | required
  Bool ->
  -- | rows
  Int ->
  -- | maxLength
  Maybe Int ->
  -- | classes (normal state)
  Text ->
  -- | error classes (invalid state)
  Text ->
  -- | disabled
  Bool ->
  Lucid.Html ()
textareaWithValidation fieldName name mId mValue mPlaceholder required rows mMaxLen classes errorClasses disabled =
  Lucid.textarea_
    ( [ Lucid.name_ name,
        Lucid.rows_ (display rows),
        xModel_ [i|fields.#{fieldName}.value|],
        xBindClass_ [i|showErrors && !fields.#{fieldName}.isValid ? '#{errorClasses}' : '#{classes}'|],
        xOnInput_ [i|showErrors && validate#{capitalizeFirst fieldName}()|],
        xOnBlur_ [i|showErrors && validate#{capitalizeFirst fieldName}()|],
        Lucid.class_ classes
      ]
        <> optionalAttr Lucid.id_ mId
        <> optionalAttr Lucid.placeholder_ mPlaceholder
        <> boolAttr (Lucid.required_ "") required
        <> optionalAttrWith Lucid.maxlength_ mMaxLen
        <> boolAttr (Lucid.disabled_ "") disabled
    )
    (maybe mempty Lucid.toHtml mValue)

--------------------------------------------------------------------------------
-- Select fields

data SelectOption = SelectOption
  { soValue :: Text,
    soLabel :: Text,
    soSelected :: Bool,
    soDescription :: Maybe Text
  }

-- | Simple select dropdown
select ::
  -- | name
  Text ->
  -- | id
  Maybe Text ->
  -- | required
  Bool ->
  -- | classes
  Text ->
  -- | options
  [SelectOption] ->
  -- | disabled
  Bool ->
  Lucid.Html ()
select name mId required classes options disabled =
  Lucid.select_
    ( [ Lucid.name_ name,
        Lucid.class_ classes
      ]
        <> optionalAttr Lucid.id_ mId
        <> boolAttr (Lucid.required_ "") required
        <> boolAttr (Lucid.disabled_ "") disabled
    )
    $ mapM_ renderOption options
  where
    renderOption :: SelectOption -> Lucid.Html ()
    renderOption opt =
      Lucid.option_
        ([Lucid.value_ opt.soValue] <> boolAttr (Lucid.selected_ "") opt.soSelected)
        (Lucid.toHtml opt.soLabel)

--------------------------------------------------------------------------------
-- Number input fields

-- | Simple number input
numberInput ::
  -- | name
  Text ->
  -- | id
  Maybe Text ->
  -- | value
  Maybe Int ->
  -- | min
  Maybe Int ->
  -- | max
  Maybe Int ->
  -- | step
  Maybe Int ->
  -- | required
  Bool ->
  -- | classes
  Text ->
  -- | disabled
  Bool ->
  Lucid.Html ()
numberInput name mId mValue mMin mMax mStep required classes disabled =
  Lucid.input_ $
    [ Lucid.type_ "number",
      Lucid.name_ name,
      Lucid.class_ classes
    ]
      <> optionalAttr Lucid.id_ mId
      <> optionalAttrWith Lucid.value_ mValue
      <> optionalAttrWith Lucid.min_ mMin
      <> optionalAttrWith Lucid.max_ mMax
      <> optionalAttrWith Lucid.step_ mStep
      <> boolAttr (Lucid.required_ "") required
      <> boolAttr (Lucid.disabled_ "") disabled

--------------------------------------------------------------------------------
-- Hidden input fields

hiddenInput :: Text -> Text -> Lucid.Html ()
hiddenInput name value =
  Lucid.input_
    [ Lucid.type_ "hidden",
      Lucid.name_ name,
      Lucid.value_ value
    ]

--------------------------------------------------------------------------------
-- Checkbox fields

-- | Simple checkbox input
checkbox ::
  -- | name
  Text ->
  -- | id
  Maybe Text ->
  -- | value
  Text ->
  -- | checked
  Bool ->
  -- | classes
  Text ->
  -- | disabled
  Bool ->
  Lucid.Html ()
checkbox name mId value checked classes disabled =
  Lucid.input_ $
    [ Lucid.type_ "checkbox",
      Lucid.name_ name,
      Lucid.value_ value,
      Lucid.class_ classes
    ]
      <> optionalAttr Lucid.id_ mId
      <> boolAttr Lucid.checked_ checked
      <> boolAttr (Lucid.disabled_ "") disabled

--------------------------------------------------------------------------------
-- File input fields

-- | Simple file input
fileInput ::
  -- | name
  Text ->
  -- | id
  Maybe Text ->
  -- | accept
  Maybe Text ->
  -- | required
  Bool ->
  -- | classes
  Text ->
  -- | onChange
  Maybe Text ->
  Lucid.Html ()
fileInput name mId mAccept required classes mOnChange =
  Lucid.input_ $
    [ Lucid.type_ "file",
      Lucid.name_ name,
      Lucid.class_ classes
    ]
      <> optionalAttr Lucid.id_ mId
      <> optionalAttr Lucid.accept_ mAccept
      <> boolAttr (Lucid.required_ "") required
      <> optionalAttr xOnChange_ mOnChange

--------------------------------------------------------------------------------
-- Helpers

-- | Capitalize first letter of text
capitalizeFirst :: Text -> Text
capitalizeFirst t = case Text.uncons t of
  Nothing -> ""
  Just (c, rest) -> Text.cons (Data.Char.toUpper c) rest
