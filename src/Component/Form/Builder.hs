{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Component.Form.Builder
  ( -- * Form Builder Types
    FormBuilder (..),
    FormField (..),
    FormStyles (..),
    ValidationRules (..),
    SelectOption (..),
    HtmxConfig (..),
    emptyValidation,
    defaultFormStyles,

    -- * Building Forms
    buildValidatedForm,
    renderField,
  )
where

--------------------------------------------------------------------------------

import Component.Form.Internal
  ( SelectOption (..),
    capitalizeFirst,
    hiddenInput,
    select,
    textInputWithValidation,
    textareaWithValidation,
  )
import Data.Char qualified
import Data.Either (lefts)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Lucid qualified
import Lucid.Base (makeAttributes)
import Lucid.Extras (hxPost_, hxSwap_, hxTarget_, xBindClass_, xBindSrc_, xData_, xModel_, xOnChange_, xOnClick_, xOnSubmit_, xShow_, xText_)

--------------------------------------------------------------------------------

-- | CSS classes for form theming
-- All fields use Tailwind CSS classes and can be easily customized
data FormStyles = FormStyles
  { -- | Input field styles
    fsTextInputClasses :: Text,
    fsTextInputErrorClasses :: Text,
    -- | Textarea styles
    fsTextareaClasses :: Text,
    fsTextareaErrorClasses :: Text,
    -- | Select dropdown styles
    fsSelectClasses :: Text,
    -- | File upload styles
    fsFileUploadClasses :: Text,
    fsFileUploadErrorClasses :: Text,
    -- | Label and text styles
    fsLabelClasses :: Text,
    fsErrorMessageClasses :: Text,
    fsHintClasses :: Text,
    -- | Section styles
    fsSectionTitleClasses :: Text,
    fsSectionContentClasses :: Text,
    -- | Form container styles
    fsFormClasses :: Text
  }

-- | Default form styles with Tailwind CSS classes
defaultFormStyles :: FormStyles
defaultFormStyles =
  FormStyles
    { fsTextInputClasses = "w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600",
      fsTextInputErrorClasses = "w-full p-3 border-2 border-red-500 font-mono focus:border-red-600",
      fsTextareaClasses = "w-full p-3 border-2 border-gray-400 font-mono leading-relaxed focus:border-blue-600",
      fsTextareaErrorClasses = "w-full p-3 border-2 border-red-500 font-mono leading-relaxed focus:border-red-600",
      fsSelectClasses = "w-full p-3 border-2 border-gray-400 font-mono",
      fsFileUploadClasses = "border-2 border-dashed border-gray-400 p-6 text-center",
      fsFileUploadErrorClasses = "border-2 border-dashed border-red-500 p-6 text-center",
      fsLabelClasses = "block font-bold mb-2",
      fsErrorMessageClasses = "mt-1 text-sm text-red-600",
      fsHintClasses = "text-xs text-gray-600 mt-1",
      fsSectionTitleClasses = "text-xl font-bold mb-4 border-b border-gray-800 pb-2",
      fsSectionContentClasses = "space-y-6",
      fsFormClasses = "space-y-8 w-full"
    }

-- | Validation rules for a form field
data ValidationRules = ValidationRules
  { vrMinLength :: Maybe Int,
    vrMaxLength :: Maybe Int,
    vrPattern :: Maybe Text,
    vrRequired :: Bool,
    -- | Custom JavaScript validation expression (must return boolean)
    -- The expression has access to 'this.fields' and current field value via 'trimmed'
    vrCustomValidation :: Maybe Text
  }

-- | Empty validation (no rules)
emptyValidation :: ValidationRules
emptyValidation =
  ValidationRules
    { vrMinLength = Nothing,
      vrMaxLength = Nothing,
      vrPattern = Nothing,
      vrRequired = False,
      vrCustomValidation = Nothing
    }

-- | A form field with validation
data FormField
  = ValidatedTextField
      { vfName :: Text,
        vfLabel :: Text,
        vfInitialValue :: Maybe Text,
        vfPlaceholder :: Maybe Text,
        vfHint :: Maybe Text,
        vfValidation :: ValidationRules
      }
  | ValidatedTextareaField
      { vtName :: Text,
        vtLabel :: Text,
        vtInitialValue :: Maybe Text,
        vtRows :: Int,
        vtPlaceholder :: Maybe Text,
        vtHint :: Maybe Text,
        vtValidation :: ValidationRules
      }
  | ValidatedSelectField
      { vsName :: Text,
        vsLabel :: Text,
        vsOptions :: [SelectOption],
        vsHint :: Maybe Text,
        vsValidation :: ValidationRules
      }
  | HiddenField
      { hfName :: Text,
        hfValue :: Text
      }
  | SectionField
      { sfTitle :: Text,
        sfFields :: [FormField]
      }
  | PlainField
      { pfHtml :: Lucid.Html ()
      }
  | ConditionalField
      { cfCondition :: Bool,
        cfTrueFields :: [FormField],
        cfFalseFields :: [FormField]
      }
  | ValidatedFileField
      { vffName :: Text,
        vffLabel :: Text,
        vffAccept :: Maybe Text,
        vffHint :: Maybe Text,
        vffMaxSizeMB :: Maybe Int,
        vffValidation :: ValidationRules,
        vffButtonText :: Text,
        vffButtonClasses :: Text,
        -- | Display the current file URL/path if file already uploaded
        vffCurrentValue :: Maybe Text
      }
  | ValidatedDateTimeField
      { vdtName :: Text,
        vdtLabel :: Text,
        vdtInitialValue :: Maybe Text,
        vdtHint :: Maybe Text,
        vdtValidation :: ValidationRules
      }
  | ValidatedRadioField
      { vrfName :: Text,
        vrfLabel :: Text,
        vrfOptions :: [SelectOption],
        vrfHint :: Maybe Text,
        vrfValidation :: ValidationRules
      }

-- | HTMX configuration for form submission
data HtmxConfig = HtmxConfig
  { -- | Target element for response (e.g., "#main-content")
    hcTarget :: Text,
    -- | Optional swap strategy (default: innerHTML)
    hcSwap :: Maybe Text
  }

-- | Form builder configuration
data FormBuilder = FormBuilder
  { fbAction :: Text,
    fbMethod :: Text,
    fbHeader :: Maybe (Lucid.Html ()),
    fbFields :: [FormField],
    fbAdditionalContent :: [Lucid.Html ()],
    fbStyles :: FormStyles,
    -- | Optional HTMX config for progressive enhancement
    fbHtmx :: Maybe HtmxConfig
  }

--------------------------------------------------------------------------------
-- Alpine State Generation

-- | Validate that a field name is a safe JavaScript identifier
-- Only allows alphanumeric characters and underscores, must start with letter or underscore
-- This prevents JavaScript injection through malicious field names
validateFieldName :: Text -> Either Text Text
validateFieldName name
  | Text.null name = Left "Field name cannot be empty"
  | not (isValidStart (Text.head name)) = Left [i|Field name must start with letter or underscore: '#{name}'|]
  | not (Text.all isValidChar name) = Left [i|Field name contains invalid characters: '#{name}'|]
  | otherwise = Right name
  where
    isValidStart c = Data.Char.isAlpha c || c == '_'
    isValidChar c = Data.Char.isAlphaNum c || c == '_'

-- | Escape a string for safe use in JavaScript string literals
-- Escapes: backslash, single quote, double quote, newline, carriage return, tab
escapeJsString :: Text -> Text
escapeJsString = Text.concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '\'' = "\\'"
    escapeChar '"' = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = Text.singleton c

-- | Get the name from a FormField for validation
-- Returns Nothing for fields without names (PlainField, etc.)
getFieldName :: FormField -> Maybe Text
getFieldName (ValidatedTextField {vfName = name}) = Just name
getFieldName (ValidatedTextareaField {vtName = name}) = Just name
getFieldName (ValidatedSelectField {vsName = name}) = Just name
getFieldName (ValidatedFileField {vffName = name}) = Just name
getFieldName (ValidatedDateTimeField {vdtName = name}) = Just name
getFieldName (ValidatedRadioField {vrfName = name}) = Just name
getFieldName (HiddenField {hfName = name}) = Just name
getFieldName (SectionField {}) = Nothing
getFieldName (PlainField {}) = Nothing
getFieldName (ConditionalField {}) = Nothing

-- | Validate all field names in a form
-- Returns list of validation errors
validateFormFieldNames :: [FormField] -> [Text]
validateFormFieldNames fields =
  let allFields = flattenFields fields
      namedFields = mapMaybe getFieldName allFields
      validations = map validateFieldName namedFields
   in lefts validations

-- | Flatten nested fields (recursively extract from sections and conditionals)
flattenFields :: [FormField] -> [FormField]
flattenFields = concatMap flatten
  where
    flatten (SectionField {sfFields = fields}) = flattenFields fields
    flatten (ConditionalField {cfCondition = cond, cfTrueFields = trueFields, cfFalseFields = falseFields}) =
      flattenFields $ if cond then trueFields else falseFields
    flatten field = [field]

-- | Generate Alpine.js state for validated fields
-- Takes pre-flattened fields for efficiency
generateAlpineState :: [FormField] -> Text
generateAlpineState allFields =
  let validatedFields = filter isValidated allFields
      fieldInits = Text.intercalate ",\n    " $ map generateFieldInit validatedFields
      validators = Text.intercalate ",\n\n  " $ map generateValidator validatedFields
      validatorCalls = Text.intercalate ";\n    " $ map (\f -> [i|this.validate#{capitalizeFieldName f}()|]) validatedFields
   in [i|{
  fields: {
    #{fieldInits}
  },
  showErrors: false,

  #{validators},

  validateAndSubmit(event) {
    event.preventDefault();
    event.stopPropagation();

    this.showErrors = true;

    #{validatorCalls};

    const allFieldsValid = Object.values(this.fields).every(field => field.isValid);

    if (!allFieldsValid) {
      // Find first invalid field and focus it
      const firstInvalidFieldName = Object.keys(this.fields).find(name => !this.fields[name].isValid);
      if (firstInvalidFieldName) {
        const fieldElement = document.getElementById(firstInvalidFieldName) ||
                            document.querySelector(`[name="${firstInvalidFieldName}"]`);
        if (fieldElement) {
          fieldElement.scrollIntoView({ behavior: 'smooth', block: 'center' });
          setTimeout(() => fieldElement.focus(), 300);
        }
      }

      return false;
    }

    // Validation passed - use HTMX if available, otherwise fallback to regular submit
    if (typeof htmx !== 'undefined' && event.target.hasAttribute('hx-post')) {
      htmx.trigger(event.target, 'submit');
    } else {
      event.target.submit();
    }
    return true;
  },

  formatFileSize(bytes) {
    if (bytes === 0) return '0 Bytes';
    const k = 1024;
    const sizes = ['Bytes', 'KB', 'MB', 'GB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return Math.round(bytes / Math.pow(k, i) * 100) / 100 + ' ' + sizes[i];
  }
}|]

isValidated :: FormField -> Bool
isValidated (ValidatedTextField {}) = True
isValidated (ValidatedTextareaField {}) = True
isValidated (ValidatedSelectField {}) = False -- Selects typically don't need Alpine validation
isValidated (HiddenField {}) = False
isValidated (SectionField {}) = False
isValidated (PlainField {}) = False
isValidated (ConditionalField {}) = False
isValidated (ValidatedFileField {}) = True
isValidated (ValidatedDateTimeField {}) = True
isValidated (ValidatedRadioField {}) = False -- Radio groups are validated via HTML required attribute

isFileField :: FormField -> Bool
isFileField (ValidatedFileField {}) = True
isFileField (SectionField {sfFields = fields}) = any isFileField fields
isFileField (ConditionalField {cfTrueFields = trueFields, cfFalseFields = falseFields}) =
  any isFileField trueFields || any isFileField falseFields
isFileField _ = False

capitalizeFieldName :: FormField -> Text
capitalizeFieldName (ValidatedTextField {vfName = name}) = capitalizeFirst name
capitalizeFieldName (ValidatedTextareaField {vtName = name}) = capitalizeFirst name
capitalizeFieldName (ValidatedSelectField {vsName = name}) = capitalizeFirst name
capitalizeFieldName (ValidatedFileField {vffName = name}) = capitalizeFirst name
capitalizeFieldName (ValidatedDateTimeField {vdtName = name}) = capitalizeFirst name
capitalizeFieldName (ValidatedRadioField {vrfName = name}) = capitalizeFirst name
capitalizeFieldName (HiddenField {}) = ""
capitalizeFieldName (SectionField {}) = ""
capitalizeFieldName (PlainField {}) = ""
capitalizeFieldName (ConditionalField {}) = ""

-- | Generate field initialization JavaScript
generateFieldInit :: FormField -> Text
generateFieldInit (ValidatedTextField {vfName = name, vfInitialValue = val}) =
  let v = escapeJsString (fromMaybe "" val)
   in [i|#{name}: { value: `#{v}`, isValid: true }|]
generateFieldInit (ValidatedTextareaField {vtName = name, vtInitialValue = val}) =
  let v = escapeJsString (fromMaybe "" val)
   in [i|#{name}: { value: `#{v}`, isValid: true }|]
generateFieldInit (ValidatedFileField {vffName = name, vffCurrentValue = currentValue}) =
  let hasCurrentFile :: Text
      hasCurrentFile = case currentValue of
        Just _ -> "false"
        Nothing -> "true" -- No current file, so effectively already "cleared"
   in [i|#{name}: { fileName: '', fileSize: 0, isValid: true, error: '', previewUrl: '', currentCleared: #{hasCurrentFile} }|]
generateFieldInit (ValidatedDateTimeField {vdtName = name, vdtInitialValue = val}) =
  let v = escapeJsString (fromMaybe "" val)
   in [i|#{name}: { value: `#{v}`, isValid: true }|]
generateFieldInit (ValidatedRadioField {}) = ""
generateFieldInit (ValidatedSelectField {}) = ""
generateFieldInit (HiddenField {}) = ""
generateFieldInit (SectionField {}) = ""
generateFieldInit (PlainField {}) = ""
generateFieldInit (ConditionalField {}) = ""

-- | Generate validator JavaScript functions
generateValidator :: FormField -> Text
generateValidator (ValidatedTextField {vfName = name, vfValidation = rules}) =
  let funcName :: Text = [i|validate#{capitalizeFirst name}|]
      conditions = generateValidationConditions name rules
   in [i|#{funcName}() {
    const trimmed = this.fields.#{name}.value.trim();
    this.fields.#{name}.isValid = #{conditions};
  }|]
generateValidator (ValidatedTextareaField {vtName = name, vtValidation = rules}) =
  let funcName :: Text = [i|validate#{capitalizeFirst name}|]
      conditions = generateValidationConditions name rules
   in [i|#{funcName}() {
    const trimmed = this.fields.#{name}.value.trim();
    this.fields.#{name}.isValid = #{conditions};
  }|]
generateValidator (ValidatedFileField {vffName = name, vffMaxSizeMB = maxSize, vffAccept = accept, vffValidation = rules}) =
  let capitalizedName = capitalizeFirst name
      requiredCheck :: Text =
        if vrRequired rules
          then
            [i|
    if (!file) {
      this.fields.#{name}.isValid = false;
      this.fields.#{name}.error = 'This field is required';
      return;
    }|]
          else
            [i|
    if (!file) {
      this.fields.#{name}.isValid = true;
      this.fields.#{name}.error = '';
      return;
    }|]
      sizeCheck :: Text = case maxSize of
        Nothing -> ""
        Just mb ->
          [i|
    if (file.size > #{mb * 1024 * 1024}) {
      this.fields.#{name}.isValid = false;
      this.fields.#{name}.error = 'File size must be less than #{mb}MB';
      return;
    }|]
      acceptCheck :: Text = case accept of
        Nothing -> ""
        Just acceptTypes ->
          [i|
    const acceptedTypes = '#{acceptTypes}'.split(',').map(t => t.trim());
    const fileType = file.type;
    const isAccepted = acceptedTypes.some(type => {
      if (type.endsWith('/' + '*')) {
        const prefix = type.slice(0, -2);
        return fileType.startsWith(prefix + '/');
      }
      return fileType === type;
    });
    if (!isAccepted) {
      this.fields.#{name}.isValid = false;
      this.fields.#{name}.error = 'Invalid file type. Accepted: #{acceptTypes}';
      return;
    }|]
   in [i|validate#{capitalizedName}() {
    const input = document.getElementById('#{name}-input');
    const file = input?.files?.[0];#{requiredCheck}#{sizeCheck}#{acceptCheck}
    this.fields.#{name}.isValid = true;
    this.fields.#{name}.error = '';
  },

  handle#{capitalizedName}Change(event) {
    const file = event.target.files?.[0];
    // Revoke old preview URL to prevent memory leaks
    if (this.fields.#{name}.previewUrl) {
      URL.revokeObjectURL(this.fields.#{name}.previewUrl);
      this.fields.#{name}.previewUrl = '';
    }
    if (file) {
      this.fields.#{name}.fileName = file.name;
      this.fields.#{name}.fileSize = file.size;
      // Create preview URL for image files
      if (file.type.startsWith('image/')) {
        this.fields.#{name}.previewUrl = URL.createObjectURL(file);
      }
    } else {
      this.fields.#{name}.fileName = '';
      this.fields.#{name}.fileSize = 0;
    }
    if (this.showErrors) {
      this.validate#{capitalizedName}();
    }
  },

  clear#{capitalizedName}() {
    const input = document.getElementById('#{name}-input');
    if (input) {
      input.value = '';
    }
    // Revoke preview URL to prevent memory leaks
    if (this.fields.#{name}.previewUrl) {
      URL.revokeObjectURL(this.fields.#{name}.previewUrl);
    }
    this.fields.#{name}.fileName = '';
    this.fields.#{name}.fileSize = 0;
    this.fields.#{name}.previewUrl = '';
    this.fields.#{name}.currentCleared = true;
    if (this.showErrors) {
      this.validate#{capitalizedName}();
    }
  }|]
generateValidator (ValidatedDateTimeField {vdtName = name, vdtValidation = rules}) =
  let funcName :: Text = [i|validate#{capitalizeFirst name}|]
      conditions = generateValidationConditions name rules
   in [i|#{funcName}() {
    const trimmed = this.fields.#{name}.value.trim();
    this.fields.#{name}.isValid = #{conditions};
  }|]
generateValidator (ValidatedRadioField {}) = ""
generateValidator (ValidatedSelectField {}) = ""
generateValidator (HiddenField {}) = ""
generateValidator (SectionField {}) = ""
generateValidator (PlainField {}) = ""
generateValidator (ConditionalField {}) = ""

generateValidationConditions :: Text -> ValidationRules -> Text
generateValidationConditions _ rules =
  let conditions =
        concat
          [ case vrMinLength rules of
              Nothing -> []
              Just minLen -> [[i|trimmed.length >= #{minLen}|]],
            case vrMaxLength rules of
              Nothing -> []
              Just maxLen -> [[i|trimmed.length <= #{maxLen}|]],
            [[i|trimmed.length > 0|] | vrRequired rules],
            case vrCustomValidation rules of
              Nothing -> []
              Just customExpr -> [[i|(#{customExpr})|]]
          ]
   in if null conditions
        then "true"
        else Text.intercalate " && " conditions

--------------------------------------------------------------------------------
-- Form Rendering

-- | Build a form with automatic Alpine.js validation state
-- Validates all field names are safe JavaScript identifiers before rendering
buildValidatedForm :: FormBuilder -> Lucid.Html ()
buildValidatedForm builder = do
  let allFields = flattenFields (fbFields builder)
      validationErrors = validateFormFieldNames (fbFields builder)

  -- Check for field name validation errors
  -- If any exist, render an error message instead of the form
  if not (null validationErrors)
    then Lucid.div_ [Lucid.class_ "p-4 bg-red-100 border-2 border-red-500 text-red-900"] $ do
      Lucid.h3_ [Lucid.class_ "font-bold mb-2"] "Form Configuration Error"
      Lucid.ul_ [Lucid.class_ "list-disc pl-5"] $
        mapM_ (Lucid.li_ . Lucid.toHtml) validationErrors
    else do
      let alpineState = generateAlpineState allFields
          hasValidation = any isValidated allFields

      -- Render header first (outside form element)
      fromMaybe mempty (fbHeader builder)

      -- Render form with or without Alpine validation wrapper
      if hasValidation
        then Lucid.div_ [xData_ alpineState, Lucid.class_ "w-full"] $ do
          renderFormElement builder allFields
        else renderFormElement builder allFields

renderFormElement :: FormBuilder -> [FormField] -> Lucid.Html ()
renderFormElement builder allFields = do
  let hasValidation = any isValidated allFields
      hasFileFields = any isFileField allFields
      styles = fbStyles builder
      -- Default HTMX config: target #main-content with innerHTML swap
      defaultHtmxConfig = HtmxConfig "#main-content" Nothing
      htmxConfig = fromMaybe defaultHtmxConfig (fbHtmx builder)
      htmxAttrs =
        [ hxPost_ (fbAction builder),
          hxTarget_ (hcTarget htmxConfig)
        ]
          <> maybe [] (\s -> [hxSwap_ s]) (hcSwap htmxConfig)
  Lucid.form_
    ( [ Lucid.action_ (fbAction builder),
        Lucid.method_ (fbMethod builder),
        Lucid.class_ (fsFormClasses styles)
      ]
        <> ([Lucid.enctype_ "multipart/form-data" | hasFileFields])
        <> htmxAttrs
        <> (if hasValidation then [xOnSubmit_ "validateAndSubmit($event)", makeAttributes "novalidate" ""] else [])
    )
    $ do
      mapM_ (renderField styles) (fbFields builder)
      sequence_ (fbAdditionalContent builder)

renderField :: FormStyles -> FormField -> Lucid.Html ()
renderField styles (ValidatedTextField {vfName = name, vfLabel = label, vfInitialValue = initialValue, vfPlaceholder = placeholder, vfHint = hint, vfValidation = rules}) = do
  Lucid.div_ $ do
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml $ if vrRequired rules then label <> " *" else label)
    textInputWithValidation
      name -- fieldName (for Alpine binding)
      name -- name
      (Just name) -- id
      initialValue -- value
      placeholder -- placeholder
      (vrRequired rules) -- required
      (vrMinLength rules) -- minLength
      (vrMaxLength rules) -- maxLength
      Nothing -- pattern
      Nothing -- title
      (fsTextInputClasses styles) -- classes (normal state)
      (fsTextInputErrorClasses styles) -- error classes (invalid state)
      False -- disabled
      -- Show validation error message (hidden by default, shown by Alpine)
    Lucid.div_
      [ xShow_ [i|!fields.#{name}.isValid && showErrors|],
        Lucid.class_ (fsErrorMessageClasses styles),
        Lucid.style_ "display: none;"
      ]
      [i|Please check this field|]
    case hint of
      Nothing -> mempty
      Just h -> Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)
renderField styles (ValidatedTextareaField {vtName = name, vtLabel = label, vtInitialValue = initialValue, vtRows = rows, vtPlaceholder = placeholder, vtHint = hint, vtValidation = rules}) = do
  Lucid.div_ $ do
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml $ if vrRequired rules then label <> " *" else label)
    textareaWithValidation
      name -- fieldName (for Alpine binding)
      name -- name
      (Just name) -- id
      initialValue -- value
      placeholder -- placeholder
      (vrRequired rules) -- required
      rows -- rows
      (vrMaxLength rules) -- maxLength
      (fsTextareaClasses styles) -- classes (normal state)
      (fsTextareaErrorClasses styles) -- error classes (invalid state)
      False -- disabled
      -- Show validation error message (hidden by default, shown by Alpine)
    Lucid.div_
      [ xShow_ [i|!fields.#{name}.isValid && showErrors|],
        Lucid.class_ (fsErrorMessageClasses styles),
        Lucid.style_ "display: none;"
      ]
      [i|Please check this field|]
    case hint of
      Nothing -> mempty
      Just h -> Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)
renderField styles (ValidatedSelectField {vsName = name, vsLabel = label, vsOptions = options, vsHint = hint, vsValidation = rules}) = do
  Lucid.div_ $ do
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml $ if vrRequired rules then label <> " *" else label)
    select
      name
      (Just name)
      (vrRequired rules)
      (fsSelectClasses styles)
      options
      False
    case hint of
      Nothing -> mempty
      Just h -> Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)
renderField _ (HiddenField {hfName = name, hfValue = value}) = hiddenInput name value
renderField styles (SectionField {sfTitle = title, sfFields = fields}) = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h2_ [Lucid.class_ (fsSectionTitleClasses styles)] (Lucid.toHtml title)
    Lucid.div_ [Lucid.class_ (fsSectionContentClasses styles)] $ do
      mapM_ (renderField styles) fields
renderField _ (PlainField {pfHtml = html}) = html
renderField styles (ConditionalField {cfCondition = cond, cfTrueFields = trueFields, cfFalseFields = falseFields}) = do
  mapM_ (renderField styles) $ if cond then trueFields else falseFields
renderField styles (ValidatedFileField {vffName = name, vffLabel = label, vffAccept = accept, vffHint = hint, vffValidation = rules, vffButtonText = buttonText, vffButtonClasses = buttonClasses, vffCurrentValue = currentValue}) = do
  Lucid.div_ $ do
    Lucid.label_
      [Lucid.for_ [i|#{name}-input|], Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml $ if vrRequired rules then label <> " *" else label)
    -- Show current file if one exists (hidden when cleared via Alpine.js)
    case currentValue of
      Nothing -> mempty
      Just currentFile ->
        Lucid.div_ [xShow_ [i|!fields.#{name}.currentCleared|], Lucid.class_ "mb-3 p-3 bg-gray-100 border border-gray-300"] $ do
          Lucid.div_ [Lucid.class_ "text-sm mb-2 flex items-center justify-between"] $ do
            Lucid.div_ $ do
              Lucid.span_ [Lucid.class_ "font-bold text-gray-700"] "Current: "
              Lucid.a_ [Lucid.href_ currentFile, Lucid.target_ "_blank", Lucid.class_ "text-blue-600 hover:underline"] $
                Lucid.toHtml currentFile
            Lucid.button_
              [ Lucid.type_ "button",
                xOnClick_ [i|fields.#{name}.currentCleared = true|],
                Lucid.class_ "px-2 py-0.5 text-xs bg-gray-200 hover:bg-gray-300 border border-gray-400 text-gray-700 font-normal"
              ]
              "Clear"
          -- Show image preview for current image files (hides on load error for non-images)
          case accept of
            Just a
              | "image" `Text.isInfixOf` a ->
                  Lucid.img_
                    [ Lucid.src_ currentFile,
                      Lucid.class_ "max-w-xs max-h-32 border border-gray-400 object-contain mt-2",
                      Lucid.alt_ "Current image",
                      makeAttributes "onerror" "this.style.display='none'"
                    ]
            _ -> mempty
    Lucid.div_
      [ xBindClass_ [i|showErrors && !fields.#{name}.isValid ? '#{fsFileUploadErrorClasses styles}' : '#{fsFileUploadClasses styles}'|]
      ]
      $ do
        Lucid.input_
          ( [ Lucid.type_ "file",
              Lucid.name_ name,
              Lucid.class_ "hidden",
              Lucid.id_ [i|#{name}-input|],
              xOnChange_ [i|handle#{capitalizeFirst name}Change($event)|]
            ]
              <> case accept of
                Just a -> [Lucid.accept_ a]
                Nothing -> []
              <> [Lucid.required_ "" | vrRequired rules]
          )
        Lucid.label_ [Lucid.for_ [i|#{name}-input|], Lucid.class_ "cursor-pointer"] $ do
          Lucid.div_ [Lucid.class_ buttonClasses] $
            Lucid.toHtml buttonText
          case hint of
            Nothing -> mempty
            Just h -> Lucid.div_ [Lucid.class_ "mt-2 text-sm text-gray-600"] (Lucid.toHtml h)
          Lucid.div_ [xShow_ [i|fields.#{name}.fileName|], Lucid.class_ "mt-2 text-sm font-bold text-gray-800 flex items-center gap-2 flex-wrap", Lucid.style_ "display: none;"] $ do
            Lucid.span_ [xText_ [i|fields.#{name}.fileName|]] ""
            Lucid.span_ [Lucid.class_ "text-gray-600"] $ do
              "("
              Lucid.span_ [xText_ [i|formatFileSize(fields.#{name}.fileSize)|]] ""
              ")"
            Lucid.button_
              [ Lucid.type_ "button",
                xOnClick_ [i|clear#{capitalizeFirst name}()|],
                Lucid.class_ "ml-2 px-2 py-0.5 text-xs bg-gray-200 hover:bg-gray-300 border border-gray-400 text-gray-700 font-normal"
              ]
              "Clear"
    -- Image preview for newly selected files
    Lucid.div_
      [ xShow_ [i|fields.#{name}.previewUrl|],
        Lucid.class_ "mt-4",
        Lucid.style_ "display: none;"
      ]
      $ do
        Lucid.p_ [Lucid.class_ "text-sm font-bold text-gray-700 mb-2"] "Preview:"
        Lucid.img_
          [ xBindSrc_ [i|fields.#{name}.previewUrl|],
            Lucid.class_ "max-w-xs max-h-48 border-2 border-gray-400 object-contain",
            Lucid.alt_ "Image preview"
          ]
    Lucid.div_
      [ xShow_ [i|!fields.#{name}.isValid && showErrors|],
        Lucid.class_ (fsErrorMessageClasses styles),
        xText_ [i|fields.#{name}.error|],
        Lucid.style_ "display: none;"
      ]
      ""
renderField styles (ValidatedDateTimeField {vdtName = name, vdtLabel = label, vdtInitialValue = initialValue, vdtHint = hint, vdtValidation = rules}) = do
  Lucid.div_ $ do
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml $ if vrRequired rules then label <> " *" else label)
    Lucid.input_
      ( [ Lucid.type_ "datetime-local",
          Lucid.name_ name,
          Lucid.id_ name,
          Lucid.class_ (fsTextInputClasses styles),
          xBindClass_ [i|showErrors && !fields.#{name}.isValid ? '#{fsTextInputErrorClasses styles}' : '#{fsTextInputClasses styles}'|],
          xModel_ [i|fields.#{name}.value|]
        ]
          <> case initialValue of
            Just val -> [Lucid.value_ val]
            Nothing -> []
          <> [Lucid.required_ "" | vrRequired rules]
      )
    Lucid.div_
      [ xShow_ [i|!fields.#{name}.isValid && showErrors|],
        Lucid.class_ (fsErrorMessageClasses styles),
        Lucid.style_ "display: none;"
      ]
      [i|Please check this field|]
    case hint of
      Nothing -> mempty
      Just h -> Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)
renderField styles (ValidatedRadioField {vrfName = name, vrfLabel = label, vrfOptions = options, vrfHint = hint, vrfValidation = rules}) = do
  Lucid.div_ $ do
    Lucid.label_
      [Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml label)
    Lucid.div_ [Lucid.class_ "space-y-3"] $ do
      mapM_ (renderRadioOption name rules) options
    case hint of
      Nothing -> mempty
      Just h -> Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)
  where
    renderRadioOption :: Text -> ValidationRules -> SelectOption -> Lucid.Html ()
    renderRadioOption radioName radioRules opt = do
      let optionId = [i|#{radioName}-#{soValue opt}|]
      Lucid.div_ $ do
        Lucid.label_ [Lucid.class_ "flex items-center gap-2 cursor-pointer"] $ do
          Lucid.input_
            ( [ Lucid.type_ "radio",
                Lucid.name_ radioName,
                Lucid.id_ optionId,
                Lucid.value_ (soValue opt)
              ]
                <> [Lucid.checked_ | soSelected opt]
                <> [Lucid.required_ "" | vrRequired radioRules]
            )
          Lucid.span_ [Lucid.class_ "font-bold"] $ Lucid.toHtml (soLabel opt)
        case soDescription opt of
          Just desc ->
            Lucid.p_ [Lucid.class_ "text-sm text-gray-600 ml-6"] $ Lucid.toHtml desc
          Nothing -> mempty
