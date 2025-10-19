module Component.Form
  ( -- * Styled Form Fields
    formTextInput,
    formNumberInput,
    formCheckbox,

    -- * Special Fields
    hiddenInput,

    -- * Configuration Types
    FormTextInputConfig (..),
    FormTextareaConfig (..),
    FormSelectConfig (..),
    FormNumberInputConfig (..),
    FormCheckboxConfig (..),
    FormFileInputConfig (..),
    SelectOption (..),
  )
where

--------------------------------------------------------------------------------

import Component.Form.Internal
import Data.Text (Text)
import Lucid qualified

--------------------------------------------------------------------------------
-- Form configuration types

data FormTextInputConfig = FormTextInputConfig
  { ftiName :: Text,
    ftiLabel :: Text,
    ftiValue :: Maybe Text,
    ftiPlaceholder :: Maybe Text,
    ftiHint :: Maybe Text,
    ftiRequired :: Bool
  }

data FormTextareaConfig = FormTextareaConfig
  { ftaName :: Text,
    ftaLabel :: Text,
    ftaValue :: Maybe Text,
    ftaRows :: Int,
    ftaPlaceholder :: Maybe Text,
    ftaHint :: Maybe Text,
    ftaRequired :: Bool
  }

data FormSelectConfig = FormSelectConfig
  { fsName :: Text,
    fsLabel :: Text,
    fsOptions :: [SelectOption],
    fsHint :: Maybe Text,
    fsRequired :: Bool
  }

data FormNumberInputConfig = FormNumberInputConfig
  { fniName :: Text,
    fniLabel :: Text,
    fniValue :: Maybe Int,
    fniMin :: Maybe Int,
    fniMax :: Maybe Int,
    fniHint :: Maybe Text,
    fniRequired :: Bool
  }

data FormCheckboxConfig = FormCheckboxConfig
  { fcName :: Text,
    fcValue :: Text,
    fcLabel :: Text,
    fcChecked :: Bool
  }

data FormFileInputConfig = FormFileInputConfig
  { ffiName :: Text,
    ffiLabel :: Text,
    ffiAccept :: Maybe Text,
    ffiHint :: Maybe Text,
    ffiRequired :: Bool
  }

--------------------------------------------------------------------------------
-- These wrappers include all styling and structure, so you only need to
-- change the styling in one place to update all forms.

-- | Standard text input field with consistent styling (no Alpine validation)
-- Usage: formTextInput FormTextInputConfig{..}
formTextInput :: FormTextInputConfig -> Lucid.Html ()
formTextInput config = do
  let FormTextInputConfig {ftiName = name, ftiLabel = label, ftiValue = value, ftiPlaceholder = placeholder, ftiHint = hint, ftiRequired = required} = config
  Lucid.div_ $ do
    -- Standard label styling (automatically add " *" if required)
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ "block font-bold mb-2"]
      (Lucid.toHtml $ if required then label <> " *" else label)
    -- Standard input styling
    textInput
      name
      (Just name) -- id
      value -- value
      placeholder -- placeholder
      required -- required
      Nothing -- minLength
      Nothing -- maxLength
      Nothing -- pattern
      Nothing -- title
      "w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600" -- classes
      False -- disabled
      -- Optional hint text with standard styling
    case hint of
      Nothing -> mempty
      Just h -> Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] (Lucid.toHtml h)

-- | Standard number input field with consistent styling
-- Usage: formNumberInput FormNumberInputConfig{..}
formNumberInput :: FormNumberInputConfig -> Lucid.Html ()
formNumberInput config = do
  let FormNumberInputConfig {fniName = name, fniLabel = label, fniValue = value, fniMin = minVal, fniMax = maxVal, fniHint = hint, fniRequired = required} = config
  Lucid.div_ $ do
    -- Standard label styling (automatically add " *" if required)
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ "block font-bold mb-2"]
      (Lucid.toHtml $ if required then label <> " *" else label)
    -- Standard number input styling
    numberInput
      name
      (Just name) -- id
      value -- value
      minVal -- min
      maxVal -- max
      Nothing -- step
      required -- required
      "w-full p-2 border border-gray-400 font-mono text-sm" -- classes
      False -- disabled
      -- Optional hint text
    case hint of
      Nothing -> mempty
      Just h -> Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] (Lucid.toHtml h)

-- | Standard checkbox field with consistent styling
-- Usage: formCheckbox FormCheckboxConfig{..}
formCheckbox :: FormCheckboxConfig -> Lucid.Html ()
formCheckbox config = do
  let FormCheckboxConfig {fcName = name, fcValue = value, fcLabel = label, fcChecked = checked} = config
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "flex items-center space-x-2"] $ do
      -- Standard checkbox styling
      checkbox
        name
        (Just name) -- id
        value -- value
        checked -- checked
        "w-4 h-4" -- classes
        False -- disabled
        -- Checkbox label
      Lucid.span_ [Lucid.class_ "text-sm font-bold"] (Lucid.toHtml label)
