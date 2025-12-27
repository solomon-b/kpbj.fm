-- | Monadic field configuration builder.
--
-- This module provides the 'FieldBuilder' monad for configuring individual
-- form fields using @do@ notation. Each function accumulates configuration
-- or validation settings.
--
-- Example:
--
-- > textField "email" do
-- >   label "Email Address"
-- >   placeholder "you@example.com"
-- >   required
-- >   pattern "[^@]+@[^@]+"
-- >   hint "We'll never share your email"
module Component.Form.Builder.Field
  ( -- * Builder Type
    FieldBuilder,
    FieldBuilderState (..),
    runFieldBuilder,

    -- * Configuration Functions
    label,
    placeholder,
    hint,
    description,
    value,
    disabled,
    classes,

    -- * File Field Configuration
    maxSize,
    buttonText,
    currentFile,
    aspectRatio,

    -- * Toggle Field Configuration
    onLabel,
    offLabel,
    onValue,
    offValue,
    checked,

    -- * Select/Radio Options
    addOption,
    addOptionSelected,
    addOptionWithDesc,
    addOptionSelectedWithDesc,

    -- * Validation Functions
    required,
    minLength,
    maxLength,
    pattern,
    customValidation,

    -- * Conditional Configuration
    when,
    unless,
  )
where

--------------------------------------------------------------------------------

import Component.Form.Builder.Types
import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Text (Text)
import Lucid qualified

--------------------------------------------------------------------------------
-- Types

-- | State accumulated while configuring a field.
data FieldBuilderState = FieldBuilderState
  { fbsConfig :: FieldConfig,
    fbsValidation :: ValidationConfig
  }
  deriving stock (Show)

instance Semigroup FieldBuilderState where
  FieldBuilderState c1 v1 <> FieldBuilderState c2 v2 =
    FieldBuilderState (mergeConfigs c1 c2) (mergeValidation v1 v2)

instance Monoid FieldBuilderState where
  mempty = FieldBuilderState defaultFieldConfig defaultValidation

-- | The field builder monad.
--
-- A 'Writer' that accumulates 'FieldBuilderState'.
type FieldBuilder = Writer FieldBuilderState ()

-- | Execute a field builder and extract the accumulated state.
runFieldBuilder :: FieldBuilder -> FieldBuilderState
runFieldBuilder = execWriter

--------------------------------------------------------------------------------
-- Internal Helpers

-- | Merge two field configs, with later values taking precedence.
mergeConfigs :: FieldConfig -> FieldConfig -> FieldConfig
mergeConfigs c1 c2 =
  FieldConfig
    { fcLabel = fcLabel c2 <|> fcLabel c1,
      fcPlaceholder = fcPlaceholder c2 <|> fcPlaceholder c1,
      fcHint = fcHint c2 <|> fcHint c1,
      fcInitialValue = fcInitialValue c2 <|> fcInitialValue c1,
      fcDisabled = fcDisabled c1 || fcDisabled c2,
      fcCustomClasses = fcCustomClasses c2 <|> fcCustomClasses c1,
      fcMaxSizeMB = fcMaxSizeMB c2 <|> fcMaxSizeMB c1,
      fcButtonText = fcButtonText c2 <|> fcButtonText c1,
      fcCurrentValue = fcCurrentValue c2 <|> fcCurrentValue c1,
      fcAspectRatio = fcAspectRatio c2 <|> fcAspectRatio c1,
      fcOptions = fcOptions c1 <> fcOptions c2,
      fcOnLabel = fcOnLabel c2 <|> fcOnLabel c1,
      fcOffLabel = fcOffLabel c2 <|> fcOffLabel c1,
      fcOnValue = fcOnValue c2 <|> fcOnValue c1,
      fcOffValue = fcOffValue c2 <|> fcOffValue c1,
      fcChecked = fcChecked c1 || fcChecked c2,
      fcDescriptionHtml = fcDescriptionHtml c2 <|> fcDescriptionHtml c1
    }
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> y = y
    x <|> _ = x

-- | Merge two validation configs.
mergeValidation :: ValidationConfig -> ValidationConfig -> ValidationConfig
mergeValidation v1 v2 =
  ValidationConfig
    { vcRequired = vcRequired v1 || vcRequired v2,
      vcMinLength = vcMinLength v2 <|> vcMinLength v1,
      vcMaxLength = vcMaxLength v2 <|> vcMaxLength v1,
      vcPattern = vcPattern v2 <|> vcPattern v1,
      vcCustomRules = vcCustomRules v1 <> vcCustomRules v2
    }
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> y = y
    x <|> _ = x

-- | Tell with just a config update.
tellConfig :: (FieldConfig -> FieldConfig) -> FieldBuilder
tellConfig f = tell $ FieldBuilderState (f defaultFieldConfig) defaultValidation

-- | Tell with just a validation update.
tellValidation :: (ValidationConfig -> ValidationConfig) -> FieldBuilder
tellValidation f = tell $ FieldBuilderState defaultFieldConfig (f defaultValidation)

--------------------------------------------------------------------------------
-- Configuration Functions

-- | Set the field label.
--
-- > textField "name" do
-- >   label "Full Name"
label :: Text -> FieldBuilder
label txt = tellConfig $ \c -> c {fcLabel = Just txt}

-- | Set placeholder text.
--
-- > textField "email" do
-- >   placeholder "you@example.com"
placeholder :: Text -> FieldBuilder
placeholder txt = tellConfig $ \c -> c {fcPlaceholder = Just txt}

-- | Set help/hint text displayed below the field.
--
-- > textField "password" do
-- >   hint "At least 8 characters"
hint :: Text -> FieldBuilder
hint txt = tellConfig $ \c -> c {fcHint = Just txt}

-- | Set rich HTML description for checkboxes.
--
-- > checkboxField "terms" do
-- >   label "I agree to the Terms"
-- >   description do
-- >     Lucid.span_ "By signing up, you agree to our "
-- >     Lucid.a_ [Lucid.href_ "/terms"] "Terms of Service"
description :: Lucid.Html () -> FieldBuilder
description html = tellConfig $ \c -> c {fcDescriptionHtml = Just html}

-- | Set the initial/default value.
--
-- > textField "country" do
-- >   value "United States"
value :: Text -> FieldBuilder
value txt = tellConfig $ \c -> c {fcInitialValue = Just txt}

-- | Mark the field as disabled.
--
-- > textField "readonly_field" do
-- >   disabled
disabled :: FieldBuilder
disabled = tellConfig $ \c -> c {fcDisabled = True}

-- | Set custom CSS classes.
--
-- > textField "special" do
-- >   classes "bg-yellow-100 border-yellow-500"
classes :: Text -> FieldBuilder
classes txt = tellConfig $ \c -> c {fcCustomClasses = Just txt}

--------------------------------------------------------------------------------
-- File Field Configuration

-- | Set maximum file size in megabytes.
--
-- > fileField "upload" (Just "image/*") do
-- >   maxSize 10
maxSize :: Int -> FieldBuilder
maxSize mb = tellConfig $ \c -> c {fcMaxSizeMB = Just mb}

-- | Set the file picker button text.
--
-- > fileField "audio" (Just "audio/*") do
-- >   buttonText "SELECT AUDIO FILE"
buttonText :: Text -> FieldBuilder
buttonText txt = tellConfig $ \c -> c {fcButtonText = Just txt}

-- | Set the URL of an existing file (for edit forms).
--
-- > fileField "avatar" (Just "image/*") do
-- >   currentFile "/uploads/avatar-123.jpg"
currentFile :: Text -> FieldBuilder
currentFile url = tellConfig $ \c -> c {fcCurrentValue = Just url}

-- | Set the aspect ratio for image cropping (width, height).
--
-- > imageField "avatar" do
-- >   aspectRatio (1, 1)  -- Square
aspectRatio :: (Int, Int) -> FieldBuilder
aspectRatio ratio = tellConfig $ \c -> c {fcAspectRatio = Just ratio}

--------------------------------------------------------------------------------
-- Toggle Field Configuration

-- | Set the label shown when the toggle is on.
--
-- > toggleField "status" do
-- >   onLabel "Published"
onLabel :: Text -> FieldBuilder
onLabel txt = tellConfig $ \c -> c {fcOnLabel = Just txt}

-- | Set the label shown when the toggle is off.
--
-- > toggleField "status" do
-- >   offLabel "Draft"
offLabel :: Text -> FieldBuilder
offLabel txt = tellConfig $ \c -> c {fcOffLabel = Just txt}

-- | Set the value submitted when the toggle is on.
--
-- > toggleField "status" do
-- >   onValue "published"
onValue :: Text -> FieldBuilder
onValue txt = tellConfig $ \c -> c {fcOnValue = Just txt}

-- | Set the value submitted when the toggle is off.
--
-- > toggleField "status" do
-- >   offValue "draft"
offValue :: Text -> FieldBuilder
offValue txt = tellConfig $ \c -> c {fcOffValue = Just txt}

-- | Set the toggle to be initially checked/on.
--
-- > toggleField "status" do
-- >   checked  -- starts in the "on" position
checked :: FieldBuilder
checked = tellConfig $ \c -> c {fcChecked = True}

--------------------------------------------------------------------------------
-- Select/Radio Options

-- | Add an option to a select or radio field.
--
-- > selectField "country" do
-- >   label "Country"
-- >   addOption "us" "United States"
-- >   addOption "ca" "Canada"
addOption :: Text -> Text -> FieldBuilder
addOption val lbl = tellConfig $ \c ->
  c {fcOptions = fcOptions c <> [SelectOption val lbl False Nothing]}

-- | Add a pre-selected option.
--
-- > selectField "status" do
-- >   addOption "draft" "Draft"
-- >   addOptionSelected "published" "Published"
addOptionSelected :: Text -> Text -> FieldBuilder
addOptionSelected val lbl = tellConfig $ \c ->
  c {fcOptions = fcOptions c <> [SelectOption val lbl True Nothing]}

-- | Add an option with a description.
--
-- > radioField "plan" do
-- >   addOptionWithDesc "free" "Free Plan" "Basic features only"
-- >   addOptionWithDesc "pro" "Pro Plan" "All features included"
addOptionWithDesc :: Text -> Text -> Text -> FieldBuilder
addOptionWithDesc val lbl desc = tellConfig $ \c ->
  c {fcOptions = fcOptions c <> [SelectOption val lbl False (Just desc)]}

-- | Add a selected option with a description.
--
-- > radioField "plan" do
-- >   addOptionSelectedWithDesc "pro" "Pro Plan" "All features included"
addOptionSelectedWithDesc :: Text -> Text -> Text -> FieldBuilder
addOptionSelectedWithDesc val lbl desc = tellConfig $ \c ->
  c {fcOptions = fcOptions c <> [SelectOption val lbl True (Just desc)]}

--------------------------------------------------------------------------------
-- Validation Functions

-- | Mark the field as required.
--
-- > textField "email" do
-- >   required
required :: FieldBuilder
required = tellValidation $ \v -> v {vcRequired = True}

-- | Set minimum length validation.
--
-- > textField "password" do
-- >   minLength 8
minLength :: Int -> FieldBuilder
minLength n = tellValidation $ \v -> v {vcMinLength = Just n}

-- | Set maximum length validation.
--
-- > textField "bio" do
-- >   maxLength 500
maxLength :: Int -> FieldBuilder
maxLength n = tellValidation $ \v -> v {vcMaxLength = Just n}

-- | Set a regex pattern for validation.
--
-- > textField "phone" do
-- >   pattern "[0-9]{3}-[0-9]{3}-[0-9]{4}"
pattern :: Text -> FieldBuilder
pattern p = tellValidation $ \v -> v {vcPattern = Just p}

-- | Add a custom validation rule.
--
-- > textField "confirm_password" do
-- >   customValidation
-- >     "this.fields.password.value === value"
-- >     "Passwords must match"
customValidation :: Text -> Text -> FieldBuilder
customValidation expr msg =
  tellValidation $ \v ->
    v {vcCustomRules = vcCustomRules v <> [CustomValidation expr msg]}

--------------------------------------------------------------------------------
-- Conditional Configuration

-- | Conditionally apply configuration.
--
-- > textField "name" do
-- >   label "Name"
-- >   when isAdmin do
-- >     hint "Admin users can edit this"
when :: Bool -> FieldBuilder -> FieldBuilder
when True builder = builder
when False _ = pure ()

-- | Conditionally apply configuration (negated).
--
-- > textField "name" do
-- >   unless isReadOnly do
-- >     placeholder "Enter your name"
unless :: Bool -> FieldBuilder -> FieldBuilder
unless cond = when (not cond)
