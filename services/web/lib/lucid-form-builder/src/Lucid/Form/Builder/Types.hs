-- | Core types for the form builder.
--
-- This module defines the data structures accumulated by the Writer monad
-- when building forms declaratively.
module Lucid.Form.Builder.Types
  ( -- * Form State
    FormState (..),
    emptyFormState,

    -- * Form Elements
    FormElement (..),

    -- * Form Footer Items
    FormFooterItem (..),

    -- * Fields
    Field (..),
    FieldType (..),
    FieldConfig (..),
    defaultFieldConfig,

    -- * Images Field Data
    ImageData (..),

    -- * Sections
    Section (..),

    -- * Select Options
    SelectOption (..),

    -- * Validation
    ValidationConfig (..),
    defaultValidation,
    CustomValidation (..),
  )
where

--------------------------------------------------------------------------------

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lucid qualified

--------------------------------------------------------------------------------
-- Form State

-- | Accumulated state from building a form.
--
-- This is what the 'FormBuilder' Writer monad accumulates.
data FormState = FormState
  { -- | Form elements (sections and fields) in order
    fsElements :: [FormElement],
    -- | Hidden fields as (name, value) pairs
    fsHiddenFields :: [(Text, Text)],
    -- | Form title (rendered as h1)
    fsTitle :: Maybe Text,
    -- | Form subtitle (rendered below title)
    fsSubtitle :: Maybe Text,
    -- | Form footer items (submit, cancel, toggles, etc.)
    fsFooterItems :: [FormFooterItem],
    -- | Optional hint text displayed below footer items
    fsFooterHint :: Maybe Text
  }
  deriving stock (Show)

instance Semigroup FormState where
  FormState e1 h1 t1 st1 f1 fh1 <> FormState e2 h2 t2 st2 f2 fh2 =
    FormState (e1 <> e2) (h1 <> h2) (t2 <|> t1) (st2 <|> st1) (f1 <> f2) (fh2 <|> fh1)

instance Monoid FormState where
  mempty = emptyFormState

-- | Empty form state.
emptyFormState :: FormState
emptyFormState = FormState {fsElements = [], fsHiddenFields = [], fsTitle = Nothing, fsSubtitle = Nothing, fsFooterItems = [], fsFooterHint = Nothing}

--------------------------------------------------------------------------------
-- Form Elements

-- | A form element - either a section or a standalone field.
--
-- This allows sections and fields to be interleaved in the order they were defined.
data FormElement
  = -- | A section with title and grouped fields
    SectionElement Section
  | -- | A standalone field (not in a section)
    FieldElement Field
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Form Footer Items

-- | An item in the form footer (buttons, toggles, etc.).
data FormFooterItem
  = -- | Submit button with label
    FooterSubmit Text
  | -- | Cancel button with URL and label
    FooterCancel Text Text
  | -- | Toggle switch rendered inline with buttons
    FooterToggle Field
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Fields

-- | A form field with its configuration and validation.
data Field = Field
  { fName :: Text,
    fType :: FieldType,
    fConfig :: FieldConfig,
    fValidation :: ValidationConfig
  }
  deriving stock (Show)

-- | Supported field types.
data FieldType
  = -- | Single-line text input
    TextField
  | -- | URL input (single-line text rendered as <input type="url">)
    UrlField
  | -- | Password input (like TextField but masked)
    PasswordField
  | -- | Multi-line text input
    TextareaField
      { tfRows :: Int
      }
  | -- | Dropdown select (options come from FieldConfig)
    SelectField
  | -- | Radio button group (options come from FieldConfig)
    RadioField
  | -- | Generic file upload
    FileField
      { ffAccept :: Maybe Text
      }
  | -- | Image upload with preview and optional cropping
    ImageField
      { ifAccept :: Maybe Text
      }
  | -- | Audio upload with player preview
    AudioField
      { afAccept :: Maybe Text
      }
  | -- | Staged audio upload (YouTube/Bandcamp style background upload)
    --
    -- The upload URL is where files are POSTed to get a token back.
    -- The token is then submitted with the form.
    StagedAudioField
      { safUploadUrl :: Text,
        safUploadType :: Text
      }
  | -- | Staged image upload (YouTube/Bandcamp style background upload)
    StagedImageField
      { sifUploadUrl :: Text,
        sifUploadType :: Text
      }
  | -- | Multi-image upload with ordering, cropping, and alt text
    ImagesField
  | -- | Date and time picker
    DateTimeField
  | -- | Numeric input
    NumberField
      { nfMin :: Maybe Int,
        nfMax :: Maybe Int,
        nfStep :: Maybe Int
      }
  | -- | Checkbox
    CheckboxField
  | -- | Toggle switch (on/off with labels)
    ToggleField
  | -- | Escape hatch for custom HTML
    PlainHtmlField
      { phfHtml :: Lucid.Html ()
      }
  deriving stock (Show)

-- | Common field configuration.
--
-- Accumulated via 'FieldBuilder' using functions like 'label', 'placeholder', etc.
data FieldConfig = FieldConfig
  { -- | Display label
    fcLabel :: Maybe Text,
    -- | Placeholder text
    fcPlaceholder :: Maybe Text,
    -- | Help text below field
    fcHint :: Maybe Text,
    -- | Pre-filled value
    fcInitialValue :: Maybe Text,
    -- | Whether field is disabled
    fcDisabled :: Bool,
    -- | Custom CSS classes
    fcCustomClasses :: Maybe Text,
    -- | Max file size in MB (file fields only)
    fcMaxSizeMB :: Maybe Int,
    -- | Button text (file fields only)
    fcButtonText :: Maybe Text,
    -- | Current file URL (file fields with existing uploads)
    fcCurrentValue :: Maybe Text,
    -- | Aspect ratio for image cropping (width, height)
    fcAspectRatio :: Maybe (Int, Int),
    -- | Options for select/radio fields
    fcOptions :: [SelectOption],
    -- | Toggle on label (e.g., "Published")
    fcOnLabel :: Maybe Text,
    -- | Toggle off label (e.g., "Draft")
    fcOffLabel :: Maybe Text,
    -- | Toggle on value (submitted when on, e.g., "published")
    fcOnValue :: Maybe Text,
    -- | Toggle off value (submitted when off, e.g., "draft")
    fcOffValue :: Maybe Text,
    -- | Initial checked/on state for toggles
    fcChecked :: Bool,
    -- | Rich description (HTML) for checkboxes
    fcDescriptionHtml :: Maybe (Lucid.Html ()),
    -- | Preview thumbnail width in pixels (imagesField only, default 150)
    fcPreviewSize :: Maybe Int,
    -- | Existing images for imagesField
    fcCurrentImages :: [ImageData]
  }
  deriving stock (Show)

-- | Default field configuration with all options unset.
defaultFieldConfig :: FieldConfig
defaultFieldConfig =
  FieldConfig
    { fcLabel = Nothing,
      fcPlaceholder = Nothing,
      fcHint = Nothing,
      fcInitialValue = Nothing,
      fcDisabled = False,
      fcCustomClasses = Nothing,
      fcMaxSizeMB = Nothing,
      fcButtonText = Nothing,
      fcCurrentValue = Nothing,
      fcAspectRatio = Nothing,
      fcOptions = [],
      fcOnLabel = Nothing,
      fcOffLabel = Nothing,
      fcOnValue = Nothing,
      fcOffValue = Nothing,
      fcChecked = False,
      fcDescriptionHtml = Nothing,
      fcPreviewSize = Nothing,
      fcCurrentImages = []
    }

--------------------------------------------------------------------------------
-- Sections

-- | A form section with a title and grouped fields.
data Section = Section
  { -- | Section title
    secTitle :: Text,
    -- | Fields within this section
    secFields :: [Field],
    -- | Optional condition for rendering
    secCondition :: Maybe Bool
  }
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Select Options

-- | An option for select or radio fields.
data SelectOption = SelectOption
  { -- | Value submitted with form
    soValue :: Text,
    -- | Display text
    soLabel :: Text,
    -- | Whether option is pre-selected
    soSelected :: Bool,
    -- | Optional description/help text
    soDescription :: Maybe Text
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Images Field Data

-- | Data for a single image in an 'imagesField'.
--
-- This type is owned by the form builder — it defines the contract between
-- the handler (which constructs values from domain data) and the Alpine.js
-- component (which consumes the JSON).
data ImageData = ImageData
  { -- | Database ID (Nothing for newly uploaded images)
    imgId :: Maybe Int64,
    -- | URL path for display (e.g. "/images/store/2026/03/25/product.jpg")
    imgUrl :: Text,
    -- | Accessibility text
    imgAltText :: Text
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON ImageData where
  toJSON img =
    Aeson.object
      [ "id" Aeson..= imgId img,
        "url" Aeson..= imgUrl img,
        "alt_text" Aeson..= imgAltText img
      ]

instance FromJSON ImageData where
  parseJSON = withObject "ImageData" $ \o ->
    ImageData
      <$> o .:? "id"
      <*> o .: "url"
      <*> o .: "alt_text"

--------------------------------------------------------------------------------
-- Validation

-- | Validation configuration for a field.
--
-- Accumulated via 'FieldBuilder' using functions like 'required', 'minLength', etc.
data ValidationConfig = ValidationConfig
  { -- | Field must have a value
    vcRequired :: Bool,
    -- | Minimum character length
    vcMinLength :: Maybe Int,
    -- | Maximum character length
    vcMaxLength :: Maybe Int,
    -- | Regex pattern to match
    vcPattern :: Maybe Text,
    -- | Custom validation rules
    vcCustomRules :: [CustomValidation]
  }
  deriving stock (Show)

-- | Default validation with no constraints.
defaultValidation :: ValidationConfig
defaultValidation =
  ValidationConfig
    { vcRequired = False,
      vcMinLength = Nothing,
      vcMaxLength = Nothing,
      vcPattern = Nothing,
      vcCustomRules = []
    }

-- | A custom validation rule with a JS expression and error message.
data CustomValidation = CustomValidation
  { -- | JavaScript expression that evaluates to boolean
    cvExpression :: Text,
    -- | Error message to display when validation fails
    cvErrorMessage :: Text
  }
  deriving stock (Show, Eq)
