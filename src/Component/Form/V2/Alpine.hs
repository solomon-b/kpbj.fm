-- | Alpine.js state generation for Form V2.
--
-- This module generates the Alpine.js x-data object for validated forms,
-- including field state initialization, validators, and helper methods.
module Component.Form.V2.Alpine
  ( -- * State Generation
    generateAlpineState,

    -- * Field Analysis
    needsValidation,
    isFileField,
    collectAllFields,
  )
where

--------------------------------------------------------------------------------

import Component.Form.V2.JS hiding (field)
import Component.Form.V2.Types
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------
-- State Generation

-- | Generate the complete Alpine.js x-data object for a form.
--
-- This generates:
-- - Field state objects with value/isValid
-- - Validator functions for each validated field
-- - File handling helpers
-- - The validateAndSubmit handler
generateAlpineState :: [Field] -> Text
generateAlpineState fields =
  let validatedFields = filter needsValidation fields
      fieldInits = generateFieldInits validatedFields
      validators = map generateValidator validatedFields
      validatorCalls = map generateValidatorCall validatedFields
      fileFields = filter isFileField fields
      fileHelpers = concatMap generateFileHelpers fileFields
      allMethods =
        validators
          <> [formatFileSizeMethod | not (null fileFields)]
          <> fileHelpers
   in renderAlpineObject fieldInits allMethods validatorCalls

--------------------------------------------------------------------------------
-- Field Analysis

-- | Check if a field needs Alpine.js validation.
needsValidation :: Field -> Bool
needsValidation field = case fType field of
  TextField -> hasValidationRules field
  TextareaField {} -> hasValidationRules field
  SelectField -> hasValidationRules field
  FileField {} -> hasValidationRules field || hasFileConstraints field
  ImageField {} -> hasValidationRules field || hasFileConstraints field
  AudioField {} -> hasValidationRules field || hasFileConstraints field
  DateTimeField -> hasValidationRules field
  NumberField {} -> hasValidationRules field
  CheckboxField -> hasValidationRules field
  RadioField -> hasValidationRules field
  ToggleField -> False -- Toggles always have a valid value
  PlainHtmlField {} -> False

-- | Check if a field has any validation rules configured.
hasValidationRules :: Field -> Bool
hasValidationRules field =
  let v = fValidation field
   in vcRequired v
        || isJust (vcMinLength v)
        || isJust (vcMaxLength v)
        || isJust (vcPattern v)
        || not (null (vcCustomRules v))

-- | Check if a file field has size constraints.
hasFileConstraints :: Field -> Bool
hasFileConstraints field = isJust (fcMaxSizeMB (fConfig field))

-- | Check if a field is a file upload field.
isFileField :: Field -> Bool
isFileField field = case fType field of
  FileField {} -> True
  ImageField {} -> True
  AudioField {} -> True
  _ -> False

-- | Collect all fields from a form state, including those in sections.
collectAllFields :: FormState -> [Field]
collectAllFields state =
  fsFields state <> concatMap secFields (fsSections state)

--------------------------------------------------------------------------------
-- Field Initialization

-- | Generate field initialization objects.
generateFieldInits :: [Field] -> Text
generateFieldInits fields =
  Text.intercalate ",\n    " (map generateFieldInit fields)

-- | Generate a single field's initial state object.
generateFieldInit :: Field -> Text
generateFieldInit field =
  let name = fName field
      cfg = fConfig field
   in case fType field of
        FileField {} ->
          let hasCurrentFile = isJust (fcCurrentValue cfg)
              clearedVal = boolToJS (not hasCurrentFile)
           in name <> ": { fileName: '', fileSize: 0, isValid: true, error: '', previewUrl: '', currentCleared: " <> clearedVal <> " }"
        _ ->
          let initialVal = escapeJsString (maybe "" id (fcInitialValue cfg))
           in name <> ": { value: '" <> initialVal <> "', isValid: true }"

boolToJS :: Bool -> Text
boolToJS True = "true"
boolToJS False = "false"

--------------------------------------------------------------------------------
-- Validator Generation

-- | Generate a validator function for a field.
generateValidator :: Field -> Text
generateValidator field =
  let name = fName field
   in case fType field of
        FileField accept -> generateFileValidator name accept field
        ImageField accept -> generateFileValidator name accept field
        AudioField accept -> generateFileValidator name accept field
        CheckboxField -> generateCheckboxValidator name field
        _ -> generateTextValidator name field

-- | Generate validator for text-like fields.
generateTextValidator :: Text -> Field -> Text
generateTextValidator name field =
  let capName = capitalizeFirst name
      val = fValidation field
      conditions = buildTextConditions val
   in "validate"
        <> capName
        <> "() {\n\
           \    const value = this.fields."
        <> name
        <> ".value;\n\
           \    const trimmed = value.trim();\n\
           \    this.fields."
        <> name
        <> ".isValid = "
        <> renderExpr conditions
        <> ";\n\
           \  }"

-- | Build validation conditions for text fields using JSExpr.
buildTextConditions :: ValidationConfig -> JSExpr
buildTextConditions val =
  let conditions =
        concat
          [ [checkRequired | vcRequired val],
            [checkMinLength n | Just n <- [vcMinLength val]],
            [checkMaxLength n | Just n <- [vcMaxLength val]],
            [checkPattern p | Just p <- [vcPattern val]],
            [raw (cvExpression cv) | cv <- vcCustomRules val]
          ]
   in case conditions of
        [] -> bool True
        _ -> allOf conditions

-- | Generate validator for file fields.
generateFileValidator :: Text -> Maybe Text -> Field -> Text
generateFileValidator name accept field =
  let capName = capitalizeFirst name
      val = fValidation field
      cfg = fConfig field
      requiredCheck =
        if vcRequired val
          then
            "\n\
            \    if (!file) {\n\
            \      this.fields."
              <> name
              <> ".isValid = false;\n\
                 \      this.fields."
              <> name
              <> ".error = 'This field is required';\n\
                 \      return;\n\
                 \    }"
          else
            "\n\
            \    if (!file) {\n\
            \      this.fields."
              <> name
              <> ".isValid = true;\n\
                 \      this.fields."
              <> name
              <> ".error = '';\n\
                 \      return;\n\
                 \    }"
      sizeCheck = case fcMaxSizeMB cfg of
        Nothing -> ""
        Just mb ->
          let maxBytes = Text.pack (show (mb * 1024 * 1024))
              mbText = Text.pack (show mb)
           in "\n\
              \    if (file.size > "
                <> maxBytes
                <> ") {\n\
                   \      this.fields."
                <> name
                <> ".isValid = false;\n\
                   \      this.fields."
                <> name
                <> ".error = 'File size must be less than "
                <> mbText
                <> "MB';\n\
                   \      return;\n\
                   \    }"
      acceptCheck = case accept of
        Nothing -> ""
        Just types ->
          "\n\
          \    const acceptedTypes = '"
            <> types
            <> "'.split(',').map(t => t.trim());\n\
               \    const isAccepted = acceptedTypes.some(type => {\n\
               \      if (type.endsWith('/*')) return file.type.startsWith(type.slice(0, -1));\n\
               \      return file.type === type;\n\
               \    });\n\
               \    if (!isAccepted) {\n\
               \      this.fields."
            <> name
            <> ".isValid = false;\n\
               \      this.fields."
            <> name
            <> ".error = 'Invalid file type';\n\
               \      return;\n\
               \    }"
   in "validate"
        <> capName
        <> "() {\n\
           \    const input = document.getElementById('"
        <> name
        <> "-input');\n\
           \    const file = input?.files?.[0];"
        <> requiredCheck
        <> sizeCheck
        <> acceptCheck
        <> "\n\
           \    this.fields."
        <> name
        <> ".isValid = true;\n\
           \    this.fields."
        <> name
        <> ".error = '';\n\
           \  }"

-- | Generate validator for checkbox fields.
generateCheckboxValidator :: Text -> Field -> Text
generateCheckboxValidator name field =
  let capName = capitalizeFirst name
      val = fValidation field
      condition =
        if vcRequired val
          then "this.fields." <> name <> ".value === true"
          else "true"
   in "validate"
        <> capName
        <> "() {\n\
           \    this.fields."
        <> name
        <> ".isValid = "
        <> condition
        <> ";\n\
           \  }"

-- | Generate a validator call for validateAndSubmit.
generateValidatorCall :: Field -> Text
generateValidatorCall field =
  let capName = capitalizeFirst (fName field)
   in "this.validate" <> capName <> "()"

--------------------------------------------------------------------------------
-- File Helpers

-- | Format file size helper method.
formatFileSizeMethod :: Text
formatFileSizeMethod =
  "formatFileSize(bytes) {\n\
  \    if (bytes === 0) return '0 Bytes';\n\
  \    const k = 1024;\n\
  \    const sizes = ['Bytes', 'KB', 'MB', 'GB'];\n\
  \    const i = Math.floor(Math.log(bytes) / Math.log(k));\n\
  \    return Math.round(bytes / Math.pow(k, i) * 100) / 100 + ' ' + sizes[i];\n\
  \  }"

-- | Generate file handling helper methods for a file field.
generateFileHelpers :: Field -> [Text]
generateFileHelpers field =
  let name = fName field
      capName = capitalizeFirst name
   in [ "handle"
          <> capName
          <> "Change(event) {\n\
             \    const file = event.target.files?.[0];\n\
             \    if (this.fields."
          <> name
          <> ".previewUrl) {\n\
             \      URL.revokeObjectURL(this.fields."
          <> name
          <> ".previewUrl);\n\
             \    }\n\
             \    if (file) {\n\
             \      this.fields."
          <> name
          <> ".fileName = file.name;\n\
             \      this.fields."
          <> name
          <> ".fileSize = file.size;\n\
             \      if (file.type.startsWith('image/')) {\n\
             \        this.fields."
          <> name
          <> ".previewUrl = URL.createObjectURL(file);\n\
             \      }\n\
             \    } else {\n\
             \      this.fields."
          <> name
          <> ".fileName = '';\n\
             \      this.fields."
          <> name
          <> ".fileSize = 0;\n\
             \    }\n\
             \    if (this.showErrors) this.validate"
          <> capName
          <> "();\n\
             \  }",
        "clear"
          <> capName
          <> "() {\n\
             \    const input = document.getElementById('"
          <> name
          <> "-input');\n\
             \    if (input) input.value = '';\n\
             \    if (this.fields."
          <> name
          <> ".previewUrl) {\n\
             \      URL.revokeObjectURL(this.fields."
          <> name
          <> ".previewUrl);\n\
             \    }\n\
             \    this.fields."
          <> name
          <> ".fileName = '';\n\
             \    this.fields."
          <> name
          <> ".fileSize = 0;\n\
             \    this.fields."
          <> name
          <> ".previewUrl = '';\n\
             \    this.fields."
          <> name
          <> ".currentCleared = true;\n\
             \    if (this.showErrors) this.validate"
          <> capName
          <> "();\n\
             \  }"
      ]

--------------------------------------------------------------------------------
-- Object Rendering

-- | Render the complete Alpine.js x-data object.
renderAlpineObject :: Text -> [Text] -> [Text] -> Text
renderAlpineObject fieldInits methods validatorCalls =
  let methodsStr = Text.intercalate ",\n\n  " methods
      callsStr = Text.intercalate ";\n    " validatorCalls
   in "{\n\
      \  fields: {\n\
      \    "
        <> fieldInits
        <> "\n\
           \  },\n\
           \  showErrors: false,\n\
           \\n\
           \  "
        <> methodsStr
        <> ",\n\
           \\n\
           \  validateAndSubmit(event) {\n\
           \    event.preventDefault();\n\
           \    this.showErrors = true;\n\
           \    "
        <> callsStr
        <> ";\n\
           \\n\
           \    const allValid = Object.values(this.fields).every(f => f.isValid);\n\
           \    if (!allValid) {\n\
           \      const firstInvalid = Object.keys(this.fields).find(k => !this.fields[k].isValid);\n\
           \      const el = document.getElementById(firstInvalid) || document.querySelector(`[name=\"${firstInvalid}\"]`);\n\
           \      if (el) {\n\
           \        el.scrollIntoView({ behavior: 'smooth', block: 'center' });\n\
           \        setTimeout(() => el.focus(), 300);\n\
           \      }\n\
           \      return false;\n\
           \    }\n\
           \\n\
           \    const form = event.target;\n\
           \    const htmxTarget = form.getAttribute('data-htmx-target');\n\
           \    const htmxAction = form.getAttribute('data-htmx-action');\n\
           \\n\
           \    if (typeof htmx !== 'undefined' && htmxTarget && htmxAction) {\n\
           \      const htmxSwap = form.getAttribute('data-htmx-swap') || 'innerHTML';\n\
           \      htmx.ajax('POST', htmxAction, {\n\
           \        source: form,\n\
           \        target: htmxTarget,\n\
           \        swap: htmxSwap,\n\
           \        values: new FormData(form)\n\
           \      });\n\
           \    } else {\n\
           \      form.submit();\n\
           \    }\n\
           \    return true;\n\
           \  }\n\
           \}"
