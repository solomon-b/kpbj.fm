{-# LANGUAGE QuasiQuotes #-}

-- | HTML rendering for form builder with semantic CSS classes.
--
-- This module converts 'FormState' to Lucid HTML, outputting semantic CSS
-- classes (fb-*) that can be styled by the consuming application.
--
-- == Semantic Class Convention
--
-- All classes use the @fb-@ prefix (form builder):
--
-- * Form structure: @fb-form@, @fb-title@, @fb-section@, etc.
-- * Fields: @fb-field@, @fb-label@, @fb-input@, @fb-textarea@, etc.
-- * States: @fb-field--error@, @fb-field--disabled@, etc.
--
-- The consuming application maps these to actual styles (e.g., Tailwind).
module Lucid.Form.Builder.Render
  ( -- * Form Configuration
    FormConfig (..),
    defaultFormConfig,

    -- * Rendering
    renderForm,
    renderField,
    renderSection,
  )
where

--------------------------------------------------------------------------------

import Control.Monad (forM_, unless, when)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Lucid qualified
import Lucid.Alpine
import Lucid.Base (makeAttributes)
import Lucid.Form.Builder.Alpine (collectAllFields, generateAlpineState, isFileField, needsValidation)
import Lucid.Form.Builder.Core (FormBuilder, runFormBuilder)
import Lucid.Form.Builder.JS (capitalizeFirst, escapeJsString)
import Lucid.Form.Builder.Types
import Lucid.HTMX

--------------------------------------------------------------------------------
-- Form Configuration

-- | Configuration for rendering a form.
data FormConfig = FormConfig
  { -- | Form action URL
    fcAction :: Text,
    -- | HTTP method (default: "post")
    fcMethod :: Text,
    -- | Optional header content above the form
    fcHeader :: Maybe (Lucid.Html ()),
    -- | Optional footer content inside the form
    fcFooter :: Maybe (Lucid.Html ()),
    -- | HTMX target selector (enables HTMX if set)
    fcHtmxTarget :: Maybe Text,
    -- | HTMX swap mode
    fcHtmxSwap :: Maybe Text
  }

-- | Default form configuration.
defaultFormConfig :: FormConfig
defaultFormConfig =
  FormConfig
    { fcAction = "",
      fcMethod = "post",
      fcHeader = Nothing,
      fcFooter = Nothing,
      fcHtmxTarget = Nothing,
      fcHtmxSwap = Nothing
    }

--------------------------------------------------------------------------------
-- Form Rendering

-- | Render a complete form from configuration and builder.
--
-- > renderForm config do
-- >   section "Details" do
-- >     textField "name" (label "Name" >> required)
renderForm :: FormConfig -> FormBuilder -> Lucid.Html ()
renderForm config builder = do
  let state = runFormBuilder builder
      allFields = collectAllFields state
      hasValidatedFields = any needsValidation allFields
      hasFileFields = any isFileField allFields
      alpineState = generateAlpineState allFields

  -- Render title/subtitle (from builder state, or fallback to config header)
  case (fsTitle state, fsSubtitle state) of
    (Nothing, Nothing) -> fromMaybe mempty (fcHeader config)
    _ -> renderFormHeader state

  -- Wrap in Alpine if validation needed
  if hasValidatedFields
    then Lucid.div_ [xData_ alpineState, Lucid.class_ "fb-form-wrapper"] $ do
      renderFormElement config state allFields hasFileFields
    else renderFormElement config state allFields hasFileFields

-- | Render the form header (title and subtitle).
renderFormHeader :: FormState -> Lucid.Html ()
renderFormHeader state =
  Lucid.header_ [Lucid.class_ "fb-header"] $ do
    forM_ (fsTitle state) $ \t ->
      Lucid.h1_ [Lucid.class_ "fb-title"] (Lucid.toHtml t)
    forM_ (fsSubtitle state) $ \st ->
      Lucid.p_ [Lucid.class_ "fb-subtitle"] (Lucid.toHtml st)

-- | Render the form element itself.
renderFormElement :: FormConfig -> FormState -> [Field] -> Bool -> Lucid.Html ()
renderFormElement config state allFields hasFileFields = do
  let hasValidation = any needsValidation allFields
      -- When validation is enabled, we DON'T use hx-post on the form because
      -- HTMX's event listener conflicts with Alpine's validation handler.
      -- Instead, we store HTMX config as data attributes and use htmx.ajax()
      -- programmatically after validation passes.
      htmxAttrs
        | hasValidation = case fcHtmxTarget config of
            Nothing -> []
            Just target ->
              [ makeAttributes "data-htmx-target" target,
                makeAttributes "data-htmx-action" (fcAction config)
              ]
                <> maybe [] (\s -> [makeAttributes "data-htmx-swap" s]) (fcHtmxSwap config)
        | otherwise = case fcHtmxTarget config of
            Nothing -> []
            Just target ->
              [ hxPost_ (fcAction config),
                hxTarget_ target
              ]
                <> maybe [] (\s -> [hxSwap_ s]) (fcHtmxSwap config)

  Lucid.form_
    ( [ Lucid.action_ (fcAction config),
        Lucid.method_ (fcMethod config),
        Lucid.class_ "fb-form"
      ]
        <> [Lucid.enctype_ "multipart/form-data" | hasFileFields]
        <> htmxAttrs
        <> if hasValidation
          then [xOnSubmit_ "validateAndSubmit($event)", makeAttributes "novalidate" ""]
          else []
    )
    $ do
      -- Hidden fields (outside spaced container so they don't affect layout)
      forM_ (fsHiddenFields state) $ \(name, val) ->
        Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ name, Lucid.value_ val]

      -- Visible form elements in a spaced container
      Lucid.div_ [Lucid.class_ "fb-form-content"] $ do
        -- Form elements (sections and fields in order)
        forM_ (fsElements state) $ \element ->
          renderElement element

        -- Footer items (from builder state, or fallback to config footer)
        case fsFooterItems state of
          [] -> fromMaybe mempty (fcFooter config)
          items -> renderFooter (fcHtmxTarget config) items (fsFooterHint state)

--------------------------------------------------------------------------------
-- Element Rendering

-- | Render a form element (section or field).
renderElement :: FormElement -> Lucid.Html ()
renderElement (SectionElement sec) = renderSection sec
renderElement (FieldElement field) = renderField field

--------------------------------------------------------------------------------
-- Section Rendering

-- | Render a form section.
renderSection :: Section -> Lucid.Html ()
renderSection sec = do
  let shouldRender = fromMaybe True (secCondition sec)
  when shouldRender $ do
    Lucid.section_ [Lucid.class_ "fb-section"] $ do
      Lucid.h2_ [Lucid.class_ "fb-section-title"] $
        Lucid.toHtml (secTitle sec)
      Lucid.div_ [Lucid.class_ "fb-section-content"] $
        forM_ (secFields sec) $ \field ->
          renderField field

--------------------------------------------------------------------------------
-- Footer Rendering

-- | Render form footer (buttons and inline controls like toggles).
renderFooter :: Maybe Text -> [FormFooterItem] -> Maybe Text -> Lucid.Html ()
renderFooter mHtmxTarget items mHint =
  Lucid.div_ [Lucid.class_ "fb-footer"] $ do
    -- Footer hint (displayed above the buttons)
    forM_ mHint $ \hint ->
      Lucid.p_ [Lucid.class_ "fb-footer-hint"] (Lucid.toHtml hint)

    -- Upload progress bar (shown during file uploads)
    Lucid.div_
      [ xShow_ "isUploading",
        Lucid.class_ "fb-progress"
      ]
      $ do
        Lucid.div_ [Lucid.class_ "fb-progress-header"] $ do
          Lucid.span_ [] "Uploading..."
          Lucid.span_ [Lucid.class_ "fb-progress-text", xText_ "uploadProgress + '%'"] ""
        Lucid.div_ [Lucid.class_ "fb-progress-track"] $
          Lucid.div_
            [ Lucid.class_ "fb-progress-bar",
              xBindStyle_ "{ width: uploadProgress + '%' }"
            ]
            mempty

    Lucid.div_ [Lucid.class_ "fb-footer-buttons"] $
      forM_ items $ \case
        FooterSubmit lbl ->
          Lucid.button_
            [ Lucid.type_ "submit",
              Lucid.class_ "fb-submit",
              xBindDisabled_ "isUploading || stagedUploading",
              xBindClass_ "{ 'fb-submit--loading': isUploading || stagedUploading }"
            ]
            (Lucid.toHtml lbl)
        FooterCancel url lbl ->
          Lucid.a_
            ( [ Lucid.href_ url,
                Lucid.class_ "fb-cancel"
              ]
                <> case mHtmxTarget of
                  Just target ->
                    [ hxGet_ url,
                      hxTarget_ target,
                      hxPushUrl_ "true"
                    ]
                  Nothing -> []
            )
            (Lucid.toHtml lbl)
        FooterToggle field ->
          renderFooterToggle field

-- | Render a toggle switch inline with footer buttons.
renderFooterToggle :: Field -> Lucid.Html ()
renderFooterToggle field = do
  let name = fName field
      cfg = fConfig field
      isDisabled = fcDisabled cfg
      isChecked = fcChecked cfg
      offLabelText = fromMaybe "Off" (fcOffLabel cfg)
      onLabelText = fromMaybe "On" (fcOnLabel cfg)
      offVal = escapeJsString $ fromMaybe "off" (fcOffValue cfg)
      onVal = escapeJsString $ fromMaybe "on" (fcOnValue cfg)
      toggleId = name <> "-toggle"
      hintAttr = maybe [] (\h -> [Lucid.title_ h]) (fcHint cfg)
      disabledClass = if isDisabled then " fb-toggle--disabled" else ""

  -- Compact inline toggle (no wrapper margin, flows with buttons)
  Lucid.div_
    ( [ xData_ [i|{ isOn: #{if isChecked then "true" else "false" :: Text} }|],
        Lucid.class_ ("fb-toggle" <> disabledClass)
      ]
        <> hintAttr
    )
    $ do
      -- Hidden input that submits the actual value
      Lucid.input_
        [ Lucid.type_ "hidden",
          Lucid.name_ name,
          xBindValue_ [i|isOn ? '#{onVal}' : '#{offVal}'|]
        ]

      -- Off label
      Lucid.span_ [Lucid.class_ "fb-toggle-off-label"] $
        Lucid.toHtml offLabelText

      -- Toggle switch
      Lucid.label_ [Lucid.class_ "fb-toggle-switch"] $ do
        Lucid.input_ $
          [ Lucid.type_ "checkbox",
            Lucid.id_ toggleId,
            Lucid.class_ "fb-toggle-input",
            xModel_ "isOn"
          ]
            <> [Lucid.disabled_ "disabled" | isDisabled]
        Lucid.div_ [Lucid.class_ "fb-toggle-track", xBindClass_ "{ 'fb-toggle--checked': isOn }"] mempty

      -- On label
      Lucid.span_ [Lucid.class_ "fb-toggle-on-label"] $
        Lucid.toHtml onLabelText

--------------------------------------------------------------------------------
-- Field Rendering

-- | Render a single form field.
renderField :: Field -> Lucid.Html ()
renderField field = case fType field of
  TextField -> renderTextField field
  PasswordField -> renderPasswordField field
  TextareaField rows -> renderTextareaField field rows
  SelectField -> renderSelectField field
  RadioField -> renderRadioField field
  FileField accept -> renderFileField field accept
  ImageField {} -> renderImageField field
  AudioField {} -> renderAudioField field
  StagedAudioField url uploadType -> renderStagedAudioField field url uploadType
  StagedImageField url uploadType -> renderStagedImageField field url uploadType
  DateTimeField -> renderDateTimeField field
  NumberField minV maxV step -> renderNumberField field minV maxV step
  CheckboxField -> renderCheckboxField field
  ToggleField -> renderToggleField field
  PlainHtmlField html -> html

--------------------------------------------------------------------------------
-- Text Field

renderTextField :: Field -> Lucid.Html ()
renderTextField field = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      isDisabled = fcDisabled cfg
      hasVal = needsValidation field && not isDisabled
      capName = capitalizeFirst name
      disabledClass = if isDisabled then " fb-field--disabled" else ""

  Lucid.div_ [Lucid.class_ ("fb-field" <> disabledClass)] $ do
    -- Label
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ "fb-label"]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq && not isDisabled then " *" else "")

    -- Input
    Lucid.input_ $
      [ Lucid.type_ "text",
        Lucid.name_ name,
        Lucid.id_ name,
        Lucid.class_ "fb-input"
      ]
        <> maybe [] (\v -> [Lucid.value_ v]) (fcInitialValue cfg)
        <> maybe [] (\p -> [Lucid.placeholder_ p]) (fcPlaceholder cfg)
        <> [Lucid.required_ "" | isReq && not isDisabled]
        <> maybe [] (\n -> [Lucid.minlength_ (Text.pack $ show n)]) (vcMinLength val)
        <> maybe [] (\n -> [Lucid.maxlength_ (Text.pack $ show n)]) (vcMaxLength val)
        <> [Lucid.disabled_ "" | isDisabled]
        <> if hasVal
          then
            [ xModel_ ("fields." <> name <> ".value"),
              xBindClass_ ("{ 'fb-input--error': showErrors && !fields." <> name <> ".isValid }"),
              xOnInput_ ("showErrors && validate" <> capName <> "()"),
              xOnBlur_ ("showErrors && validate" <> capName <> "()")
            ]
          else []

    -- Error message
    when hasVal $
      Lucid.span_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ "fb-error",
          Lucid.style_ "display: none;"
        ]
        "Please check this field"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ "fb-hint"] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- Password Field

renderPasswordField :: Field -> Lucid.Html ()
renderPasswordField field = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      isDisabled = fcDisabled cfg
      hasVal = needsValidation field && not isDisabled
      capName = capitalizeFirst name
      disabledClass = if isDisabled then " fb-field--disabled" else ""

  Lucid.div_ [Lucid.class_ ("fb-field" <> disabledClass)] $ do
    -- Label
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ "fb-label"]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq && not isDisabled then " *" else "")

    -- Input
    Lucid.input_ $
      [ Lucid.type_ "password",
        Lucid.name_ name,
        Lucid.id_ name,
        Lucid.class_ "fb-input"
      ]
        <> maybe [] (\v -> [Lucid.value_ v]) (fcInitialValue cfg)
        <> maybe [] (\p -> [Lucid.placeholder_ p]) (fcPlaceholder cfg)
        <> [Lucid.required_ "" | isReq && not isDisabled]
        <> maybe [] (\n -> [Lucid.minlength_ (Text.pack $ show n)]) (vcMinLength val)
        <> maybe [] (\n -> [Lucid.maxlength_ (Text.pack $ show n)]) (vcMaxLength val)
        <> [Lucid.disabled_ "" | isDisabled]
        <> if hasVal
          then
            [ xModel_ ("fields." <> name <> ".value"),
              xBindClass_ ("{ 'fb-input--error': showErrors && !fields." <> name <> ".isValid }"),
              xOnInput_ ("showErrors && validate" <> capName <> "()"),
              xOnBlur_ ("showErrors && validate" <> capName <> "()")
            ]
          else []

    -- Error message
    when hasVal $
      Lucid.span_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ "fb-error",
          Lucid.style_ "display: none;"
        ]
        "Please check this field"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ "fb-hint"] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- Textarea Field

renderTextareaField :: Field -> Int -> Lucid.Html ()
renderTextareaField field rows = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      isDisabled = fcDisabled cfg
      hasVal = needsValidation field && not isDisabled
      capName = capitalizeFirst name
      disabledClass = if isDisabled then " fb-field--disabled" else ""

  Lucid.div_ [Lucid.class_ ("fb-field" <> disabledClass)] $ do
    -- Label
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ "fb-label"]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq && not isDisabled then " *" else "")

    -- Textarea
    Lucid.textarea_
      ( [ Lucid.name_ name,
          Lucid.id_ name,
          Lucid.rows_ (Text.pack $ show rows),
          Lucid.class_ "fb-textarea"
        ]
          <> maybe [] (\p -> [Lucid.placeholder_ p]) (fcPlaceholder cfg)
          <> [Lucid.required_ "" | isReq && not isDisabled]
          <> maybe [] (\n -> [Lucid.minlength_ (Text.pack $ show n)]) (vcMinLength val)
          <> maybe [] (\n -> [Lucid.maxlength_ (Text.pack $ show n)]) (vcMaxLength val)
          <> [Lucid.disabled_ "" | isDisabled]
          <> if hasVal
            then
              [ xModel_ ("fields." <> name <> ".value"),
                xBindClass_ ("{ 'fb-textarea--error': showErrors && !fields." <> name <> ".isValid }"),
                xOnInput_ ("showErrors && validate" <> capName <> "()"),
                xOnBlur_ ("showErrors && validate" <> capName <> "()")
              ]
            else []
      )
      (Lucid.toHtml $ fromMaybe "" (fcInitialValue cfg))

    -- Error message
    when hasVal $
      Lucid.span_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ "fb-error",
          Lucid.style_ "display: none;"
        ]
        "Please check this field"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ "fb-hint"] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- Select Field

renderSelectField :: Field -> Lucid.Html ()
renderSelectField field = do
  let options = fcOptions (fConfig field)
      name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      isDisabled = fcDisabled cfg
      hasVal = needsValidation field && not isDisabled
      capName = capitalizeFirst name
      disabledClass = if isDisabled then " fb-field--disabled" else ""

  Lucid.div_ [Lucid.class_ ("fb-field" <> disabledClass)] $ do
    -- Label
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ "fb-label"]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq && not isDisabled then " *" else "")

    -- Select
    Lucid.select_
      ( [ Lucid.name_ name,
          Lucid.id_ name,
          Lucid.class_ "fb-select"
        ]
          <> [Lucid.required_ "" | isReq && not isDisabled]
          <> [Lucid.disabled_ "" | isDisabled]
          <> if hasVal
            then
              [ xModel_ ("fields." <> name <> ".value"),
                xBindClass_ ("{ 'fb-select--error': showErrors && !fields." <> name <> ".isValid }"),
                xOnChange_ ("showErrors && validate" <> capName <> "()")
              ]
            else []
      )
      $ do
        -- Placeholder option (only selected if no option is pre-selected)
        let hasSelectedOption = any soSelected options
        Lucid.option_ ([Lucid.value_ "", Lucid.disabled_ ""] <> [Lucid.selected_ "" | not hasSelectedOption]) $
          Lucid.toHtml $
            fromMaybe "Select..." (fcPlaceholder cfg)
        -- Options
        forM_ options $ \opt ->
          Lucid.option_
            ([Lucid.value_ (soValue opt)] <> [Lucid.selected_ "" | soSelected opt])
            (Lucid.toHtml $ soLabel opt)

    -- Error message
    when hasVal $
      Lucid.span_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ "fb-error",
          Lucid.style_ "display: none;"
        ]
        "Please select an option"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ "fb-hint"] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- Radio Field

renderRadioField :: Field -> Lucid.Html ()
renderRadioField field = do
  let options = fcOptions (fConfig field)
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      hasVal = needsValidation field
      capName = capitalizeFirst name

  Lucid.div_ [Lucid.class_ "fb-field"] $ do
    -- Label
    Lucid.p_ [Lucid.class_ "fb-label"] $
      Lucid.toHtml $
        fromMaybe name (fcLabel cfg) <> if isReq then " *" else ""

    -- Radio group
    Lucid.div_
      ( [Lucid.class_ "fb-radio-group"]
          <> [xBindClass_ ("{ 'fb-radio-group--error': showErrors && !fields." <> name <> ".isValid }") | hasVal]
      )
      $ do
        forM_ options $ \opt -> do
          let optId = name <> "-" <> soValue opt
          Lucid.label_ [Lucid.for_ optId, Lucid.class_ "fb-radio-option"] $ do
            Lucid.input_
              ( [ Lucid.type_ "radio",
                  Lucid.name_ name,
                  Lucid.id_ optId,
                  Lucid.value_ (soValue opt),
                  Lucid.class_ "fb-radio-input"
                ]
                  <> [Lucid.checked_ | soSelected opt]
                  <> [Lucid.required_ "" | isReq]
                  <> if hasVal
                    then
                      [ xModel_ ("fields." <> name <> ".value"),
                        xOnChange_ ("showErrors && validate" <> capName <> "()")
                      ]
                    else []
              )
            Lucid.span_ [Lucid.class_ "fb-radio-label"] (Lucid.toHtml $ soLabel opt)
            forM_ (soDescription opt) $ \desc ->
              Lucid.span_ [Lucid.class_ "fb-radio-description"] (Lucid.toHtml desc)

    -- Error message
    when hasVal $
      Lucid.span_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ "fb-error",
          Lucid.style_ "display: none;"
        ]
        "Please select an option"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ "fb-hint"] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- File Field

renderFileField :: Field -> Maybe Text -> Lucid.Html ()
renderFileField field accept = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      capName = capitalizeFirst name
      buttonLabel = fromMaybe "SELECT FILE" (fcButtonText cfg)

  Lucid.div_ [Lucid.class_ "fb-field"] $ do
    -- Label
    Lucid.label_
      [Lucid.class_ "fb-label"]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq then " *" else "")

    -- File upload zone
    Lucid.div_
      [ Lucid.class_ "fb-file-zone",
        xBindClass_ ("{ 'fb-file-zone--error': showErrors && !fields." <> name <> ".isValid }")
      ]
      $ do
        -- Hidden file input
        Lucid.input_
          ( [ Lucid.type_ "file",
              Lucid.name_ name,
              Lucid.id_ (name <> "-input"),
              Lucid.class_ "fb-file-input",
              xOnChange_ ("handle" <> capName <> "Change($event)")
            ]
              <> maybe [] (\a -> [Lucid.accept_ a]) accept
          )

        -- Upload button/zone
        Lucid.div_ [xShow_ ("!fields." <> name <> ".fileName"), Lucid.class_ "fb-file-empty"] $ do
          Lucid.label_
            [ Lucid.for_ (name <> "-input"),
              Lucid.class_ "fb-file-button"
            ]
            (Lucid.toHtml buttonLabel)
          forM_ (fcHint cfg) $ \h ->
            Lucid.p_ [Lucid.class_ "fb-file-hint"] (Lucid.toHtml h)

        -- Selected file display
        Lucid.div_ [xShow_ ("fields." <> name <> ".fileName"), Lucid.class_ "fb-file-preview"] $ do
          -- Image preview
          Lucid.template_ [xIf_ ("fields." <> name <> ".previewUrl")] $
            Lucid.img_
              [ xBindSrc_ ("fields." <> name <> ".previewUrl"),
                Lucid.class_ "fb-file-image"
              ]
          -- File info
          Lucid.div_ [Lucid.class_ "fb-file-info"] $ do
            Lucid.span_ [Lucid.class_ "fb-file-name", xText_ ("fields." <> name <> ".fileName")] ""
            Lucid.span_ [Lucid.class_ "fb-file-size", xText_ ("formatFileSize(fields." <> name <> ".fileSize)")] ""
          -- Clear button
          Lucid.button_
            [ Lucid.type_ "button",
              Lucid.class_ "fb-file-remove",
              xOnClick_ ("clear" <> capName <> "()")
            ]
            "Remove"

    -- Error message
    Lucid.span_
      [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
        Lucid.class_ "fb-error",
        Lucid.style_ "display: none;",
        xText_ ("fields." <> name <> ".error || 'Please select a file'")
      ]
      ""

--------------------------------------------------------------------------------
-- Image Field

renderImageField :: Field -> Lucid.Html ()
renderImageField field = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      labelText = fromMaybe name (fcLabel cfg) <> if isReq then " *" else ""
      inputId = name <> "-input"
      inputIdJs = escapeJsString inputId
      existingUrl = fromMaybe "" (fcCurrentValue cfg)
      hasExisting = existingUrl /= ""
      maxSizeMB = fromMaybe 10 (fcMaxSizeMB cfg)
      (ratioW, ratioH) = fromMaybe (1, 1) (fcAspectRatio cfg)
      aspectRatioStyle :: Text
      aspectRatioStyle = [i|aspect-ratio: #{ratioW} / #{ratioH}|]
      ratioText :: Text
      ratioText = [i|#{ratioW}:#{ratioH}|]
      ratioDecimal :: Text
      ratioDecimal = [i|#{(fromIntegral ratioW :: Double) / (fromIntegral ratioH :: Double)}|]
      accept = "image/jpeg,image/jpg,image/png,image/webp,image/gif"
      friendlyTypes :: Text
      friendlyTypes = "JPG, PNG, WEBP, GIF"

  -- Alpine.js state for this picker
  Lucid.div_
    [ xData_
        [i|{
  fileName: '',
  fileSize: 0,
  previewUrl: '',
  currentCleared: false,
  isValid: true,
  error: '',
  isDragging: false,
  showCropper: false,
  cropper: null,
  originalFile: null,
  cropImageUrl: '',

  handleFileChange(event) {
    const file = event.target.files?.[0];
    if (file && file.type.startsWith('image/')) {
      this.openCropper(file);
    }
  },

  handleDrop(event) {
    this.isDragging = false;
    const file = event.dataTransfer?.files?.[0];
    if (file && file.type.startsWith('image/')) {
      this.openCropper(file);
    }
  },

  openCropper(file) {
    if (file.size > #{maxSizeMB * 1024 * 1024}) {
      this.isValid = false;
      this.error = 'File exceeds #{maxSizeMB}MB limit';
      return;
    }
    this.originalFile = file;
    this.cropImageUrl = URL.createObjectURL(file);
    this.showCropper = true;
    this.$nextTick(() => {
      const img = this.$refs.cropperImage;
      if (img && typeof Cropper !== 'undefined') {
        this.cropper = new Cropper(img, {
          aspectRatio: #{ratioDecimal},
          viewMode: 1,
          dragMode: 'move',
          autoCropArea: 1,
          restore: false,
          guides: true,
          center: true,
          highlight: false,
          cropBoxMovable: true,
          cropBoxResizable: true,
          toggleDragModeOnDblclick: false,
        });
      }
    });
  },

  async confirmCrop() {
    if (!this.cropper) return;
    const canvas = this.cropper.getCroppedCanvas({
      maxWidth: 2048,
      maxHeight: 2048,
      imageSmoothingEnabled: true,
      imageSmoothingQuality: 'high',
    });
    const blob = await new Promise(resolve => {
      canvas.toBlob(resolve, 'image/jpeg', 0.92);
    });
    const croppedFile = new File([blob], this.originalFile.name, {
      type: 'image/jpeg',
      lastModified: Date.now(),
    });
    const input = document.getElementById('#{inputIdJs}');
    const dt = new DataTransfer();
    dt.items.add(croppedFile);
    input.files = dt.files;
    if (this.previewUrl) {
      URL.revokeObjectURL(this.previewUrl);
    }
    this.previewUrl = URL.createObjectURL(croppedFile);
    this.fileName = croppedFile.name;
    this.fileSize = croppedFile.size;
    this.currentCleared = false;
    this.isValid = true;
    this.error = '';
    this.closeCropper();
  },

  cancelCrop() {
    const input = document.getElementById('#{inputIdJs}');
    if (input) input.value = '';
    this.closeCropper();
  },

  closeCropper() {
    if (this.cropper) {
      this.cropper.destroy();
      this.cropper = null;
    }
    if (this.cropImageUrl) {
      URL.revokeObjectURL(this.cropImageUrl);
      this.cropImageUrl = '';
    }
    this.showCropper = false;
    this.originalFile = null;
  },

  clearFile() {
    const input = document.getElementById('#{inputIdJs}');
    if (input) input.value = '';
    if (this.previewUrl) {
      URL.revokeObjectURL(this.previewUrl);
    }
    this.fileName = '';
    this.fileSize = 0;
    this.previewUrl = '';
    this.currentCleared = true;
    this.validate();
  },

  validate() {
    if (#{if isReq then "true" else "false" :: Text} && this.currentCleared && !this.previewUrl) {
      this.isValid = false;
      this.error = 'This field is required';
      return;
    }
    this.isValid = true;
    this.error = '';
  },

  triggerInput() {
    document.getElementById('#{inputIdJs}').click();
  },

  formatFileSize(bytes) {
    if (bytes === 0) return '0 B';
    const k = 1024;
    const sizes = ['B', 'KB', 'MB', 'GB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return Math.round(bytes / Math.pow(k, i) * 10) / 10 + ' ' + sizes[i];
  }
}|],
      Lucid.class_ "fb-field"
    ]
    $ do
      -- Label
      Lucid.label_
        [ Lucid.for_ inputId,
          Lucid.class_ "fb-label"
        ]
        (Lucid.toHtml labelText)

      -- Hidden file input
      Lucid.input_ $
        [ Lucid.type_ "file",
          Lucid.name_ name,
          Lucid.id_ inputId,
          Lucid.accept_ accept,
          Lucid.class_ "fb-file-input",
          xOnChange_ "handleFileChange($event)"
        ]
          <> [Lucid.required_ "" | isReq]

      -- Hidden clear flag (sent when user explicitly removes an existing image)
      Lucid.input_
        [ Lucid.type_ "hidden",
          Lucid.name_ (name <> "_clear"),
          xBindValue_ "currentCleared ? 'true' : ''"
        ]

      -- Drop zone container
      Lucid.div_
        [ xBindClass_
            [i|{
            'fb-image-zone--error': !isValid,
            'fb-image-zone--dragging': isDragging && isValid,
            'fb-image-zone': !isDragging && isValid
          }|],
          xOnClick_ "triggerInput()",
          xOnDragover_ "isDragging = true",
          xOnDragleave_ "isDragging = false",
          xOnDrop_ "handleDrop($event)"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "fb-image-content"] $ do
            -- Empty/placeholder state
            let emptyCondition =
                  if hasExisting
                    then "currentCleared && !previewUrl"
                    else "!previewUrl"
                emptyStyle = if hasExisting then "display: none;" else ""

            Lucid.div_ [xShow_ emptyCondition, Lucid.style_ emptyStyle, Lucid.class_ "fb-image-empty"] $ do
              -- Aspect ratio preview box
              Lucid.div_
                [ Lucid.class_ "fb-image-placeholder",
                  Lucid.style_ aspectRatioStyle
                ]
                $ do
                  Lucid.div_ [Lucid.class_ "fb-image-icon"] $
                    Lucid.toHtmlRaw ("&#x1F5BC;" :: Text)

              -- Call to action
              Lucid.div_ [Lucid.class_ "fb-image-cta"] $ do
                Lucid.p_ [Lucid.class_ "fb-image-cta-text"] $
                  Lucid.span_ [xShow_ "!isDragging"] "Drop image here or click to browse"
                Lucid.p_ [Lucid.class_ "fb-image-cta-dragging", xShow_ "isDragging", Lucid.style_ "display: none;"] "Drop to upload"
                Lucid.p_ [Lucid.class_ "fb-image-meta"] $ do
                  Lucid.toHtml ratioText
                  Lucid.span_ [Lucid.class_ "fb-image-meta-sep"] " · "
                  Lucid.toHtml ([i|Max #{maxSizeMB}MB|] :: Text)
                  Lucid.span_ [Lucid.class_ "fb-image-meta-sep"] " · "
                  Lucid.toHtml friendlyTypes

            -- Current/existing image
            when hasExisting $
              Lucid.div_ [xShow_ "!currentCleared && !previewUrl", Lucid.class_ "fb-image-current"] $ do
                Lucid.div_
                  [ Lucid.class_ "fb-image-preview",
                    Lucid.style_ aspectRatioStyle
                  ]
                  $ do
                    Lucid.img_
                      [ Lucid.src_ existingUrl,
                        Lucid.class_ "fb-image-img",
                        Lucid.alt_ "Current image"
                      ]
                Lucid.p_ [Lucid.class_ "fb-image-current-label"] "Current image"
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "$event.stopPropagation(); clearFile()",
                    Lucid.class_ "fb-image-remove"
                  ]
                  "Remove image"

            -- New file preview
            Lucid.div_ [xShow_ "previewUrl", Lucid.style_ "display: none;", Lucid.class_ "fb-image-new"] $ do
              Lucid.div_
                [ Lucid.class_ "fb-image-preview fb-image-preview--new",
                  Lucid.style_ aspectRatioStyle
                ]
                $ do
                  Lucid.img_
                    [ xBindSrc_ "previewUrl",
                      Lucid.class_ "fb-image-img",
                      Lucid.alt_ "New image preview"
                    ]

              -- File info chip
              Lucid.div_ [Lucid.class_ "fb-image-info"] $ do
                Lucid.span_ [Lucid.class_ "fb-image-filename", xText_ "fileName"] ""
                Lucid.span_ [Lucid.class_ "fb-image-filesize"] $ do
                  "("
                  Lucid.span_ [xText_ "formatFileSize(fileSize)"] ""
                  ")"
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "$event.stopPropagation(); clearFile()",
                  Lucid.class_ "fb-image-remove"
                ]
                "Remove"

      -- Error message
      Lucid.span_
        [ xShow_ "!isValid",
          Lucid.class_ "fb-error",
          Lucid.style_ "display: none;"
        ]
        $ do
          Lucid.span_ [xText_ "error"] ""

      -- Hint for non-required with existing
      when (not isReq && hasExisting) $
        Lucid.p_
          [Lucid.class_ "fb-hint"]
          "Leave unchanged to keep the current image"

      -- Cropper Modal
      Lucid.div_
        [ xShow_ "showCropper",
          Lucid.style_ "display: none;",
          Lucid.class_ "fb-image-crop-modal"
        ]
        $ do
          -- Backdrop
          Lucid.div_
            [ xOnClick_ "cancelCrop()",
              Lucid.class_ "fb-image-crop-backdrop"
            ]
            mempty

          -- Modal content
          Lucid.div_ [Lucid.class_ "fb-image-crop-dialog"] $ do
            -- Header
            Lucid.div_ [Lucid.class_ "fb-image-crop-header"] $ do
              Lucid.h3_ [Lucid.class_ "fb-image-crop-title"] $ do
                "Crop Image"
                Lucid.span_ [Lucid.class_ "fb-image-crop-ratio"] $
                  Lucid.toHtml ([i|(#{ratioText} ratio)|] :: Text)
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "cancelCrop()",
                  Lucid.class_ "fb-image-crop-close"
                ]
                "×"

            -- Cropper container
            Lucid.div_ [Lucid.class_ "fb-image-crop-container"] $
              Lucid.img_
                [ xRef_ "cropperImage",
                  xBindSrc_ "cropImageUrl",
                  Lucid.class_ "fb-image-crop-img",
                  Lucid.alt_ "Image to crop"
                ]

            -- Footer with actions
            Lucid.div_ [Lucid.class_ "fb-image-crop-footer"] $ do
              -- Zoom controls
              Lucid.div_ [Lucid.class_ "fb-image-crop-zoom"] $ do
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "cropper?.zoom(-0.1)",
                    Lucid.class_ "fb-image-crop-zoom-btn"
                  ]
                  "−"
                Lucid.span_ [Lucid.class_ "fb-image-crop-zoom-label"] "Zoom"
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "cropper?.zoom(0.1)",
                    Lucid.class_ "fb-image-crop-zoom-btn"
                  ]
                  "+"

              -- Action buttons
              Lucid.div_ [Lucid.class_ "fb-image-crop-actions"] $ do
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "cancelCrop()",
                    Lucid.class_ "fb-image-crop-cancel"
                  ]
                  "Cancel"
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "confirmCrop()",
                    Lucid.class_ "fb-image-crop-confirm"
                  ]
                  "Apply Crop"

--------------------------------------------------------------------------------
-- Audio Field

renderAudioField :: Field -> Lucid.Html ()
renderAudioField field = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      labelText = fromMaybe name (fcLabel cfg) <> if isReq then " *" else ""
      inputId = name <> "-input"
      playerId = name <> "-player"

  Lucid.div_ [Lucid.class_ "fb-field"] $ do
    -- Label
    Lucid.label_
      [ Lucid.for_ inputId,
        Lucid.class_ "fb-label"
      ]
      (Lucid.toHtml labelText)

    -- Audio container
    Lucid.div_ [Lucid.class_ "fb-audio-container"] $ do
      -- Audio player with Alpine state
      Lucid.div_
        [ Lucid.class_ "fb-player",
          xData_ (audioPlayerAlpineScript playerId "" ("#" <> inputId))
        ]
        $ do
          Lucid.audio_ [xRef_ "audio", Lucid.preload_ "metadata"] mempty

          -- Player controls (always visible)
          Lucid.div_ [Lucid.class_ "fb-player-controls"] $ do
            Lucid.button_
              [ Lucid.type_ "button",
                Lucid.class_ "fb-player-button",
                xOnClick_ "toggle()",
                xText_ "isPlaying ? 'PAUSE' : 'PLAY'"
              ]
              "PLAY"

            Lucid.div_ [Lucid.class_ "fb-player-progress", xOnClick_ "seek($event)"] $
              Lucid.div_ [Lucid.class_ "fb-player-progress-fill", xBindStyle_ "{ width: progress + '%' }"] mempty

            Lucid.span_ [Lucid.class_ "fb-player-time", xText_ "formatTime(currentTime) + ' / ' + formatTime(duration)"] "0:00 / 0:00"

      -- Hidden file input
      Lucid.input_ $
        [ Lucid.type_ "file",
          Lucid.name_ name,
          Lucid.id_ inputId,
          Lucid.accept_ "audio/*",
          Lucid.class_ "fb-file-input"
        ]
          <> [Lucid.required_ "" | isReq]

      -- File picker button
      Lucid.label_ [Lucid.for_ inputId, Lucid.class_ "fb-audio-button-wrapper"] $ do
        Lucid.div_ [Lucid.class_ "fb-audio-button"] "CHOOSE AUDIO FILE"
        Lucid.p_ [Lucid.class_ "fb-hint"] $
          if isReq
            then "MP3, WAV, FLAC accepted"
            else "MP3, WAV, FLAC accepted · Leave empty to keep current file"

-- | Alpine.js script for the audio player, matching Component.AudioPlayer.Waveform.
audioPlayerAlpineScript :: Text -> Text -> Text -> Text
audioPlayerAlpineScript playerId' audioUrl' fileInputSelector =
  [i|{
  playerId: '#{escapeJsString playerId'}',
  isPlaying: false,
  originalUrl: '#{escapeJsString audioUrl'}',
  audioUrl: '#{escapeJsString audioUrl'}',
  currentTime: 0,
  duration: 0,
  blobUrl: null,
  fileInputSelector: '#{escapeJsString fileInputSelector}',
  init() {
    const audio = this.$refs.audio;
    audio.addEventListener('loadedmetadata', () => {
      this.duration = audio.duration;
    });
    audio.addEventListener('timeupdate', () => {
      this.currentTime = audio.currentTime;
    });
    audio.addEventListener('ended', () => {
      this.isPlaying = false;
    });

    // Watch file input for changes if selector provided
    if (this.fileInputSelector) {
      const fileInput = document.querySelector(this.fileInputSelector);
      if (fileInput) {
        fileInput.addEventListener('change', (e) => this.handleFileSelect(e));
      }
    }

    // Listen for clear events
    window.addEventListener('waveform-player-clear', (e) => {
      if (e.detail && e.detail.playerId === this.playerId) {
        this.clearAudio();
      }
    });

    // Load audio metadata if we have a URL
    if (this.audioUrl) {
      audio.src = this.audioUrl;
    }
  },
  clearAudio() {
    this.pause();
    if (this.blobUrl) {
      URL.revokeObjectURL(this.blobUrl);
      this.blobUrl = null;
    }
    this.audioUrl = '';
    this.originalUrl = '';
    const audio = this.$refs.audio;
    audio.removeAttribute('src');
    audio.load();
    this.currentTime = 0;
    this.duration = 0;
  },
  handleFileSelect(event) {
    const file = event.target.files[0];

    // Clean up old blob URL
    if (this.blobUrl) {
      URL.revokeObjectURL(this.blobUrl);
      this.blobUrl = null;
    }

    // If no file selected (cleared), reset to original URL
    if (!file) {
      this.pause();
      this.audioUrl = this.originalUrl;
      const audio = this.$refs.audio;
      if (this.originalUrl) {
        audio.src = this.originalUrl;
      } else {
        audio.removeAttribute('src');
      }
      this.currentTime = 0;
      this.duration = 0;
      return;
    }

    // Create new blob URL for the selected file
    this.blobUrl = URL.createObjectURL(file);
    this.audioUrl = this.blobUrl;

    // Reset player state
    this.pause();
    const audio = this.$refs.audio;
    audio.src = this.audioUrl;
    this.currentTime = 0;
    this.duration = 0;
  },
  toggle() {
    this.isPlaying ? this.pause() : this.play();
  },
  play() {
    if (typeof pauseOtherPlayers !== 'undefined') {
      pauseOtherPlayers(this.playerId);
    }
    const audio = this.$refs.audio;
    audio.play().then(() => { this.isPlaying = true; });
  },
  pause() {
    const audio = this.$refs.audio;
    audio.pause();
    this.isPlaying = false;
  },
  seek(event) {
    const bar = event.currentTarget;
    const rect = bar.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const percent = x / rect.width;
    const audio = this.$refs.audio;
    if (this.duration) {
      audio.currentTime = percent * this.duration;
    }
  },
  formatTime(seconds) {
    if (!seconds || isNaN(seconds)) return '0:00';
    const hours = Math.floor(seconds / 3600);
    const mins = Math.floor((seconds % 3600) / 60);
    const secs = Math.floor(seconds % 60);
    if (hours > 0) {
      return hours + ':' + (mins < 10 ? '0' : '') + mins + ':' + (secs < 10 ? '0' : '') + secs;
    }
    return mins + ':' + (secs < 10 ? '0' : '') + secs;
  },
  get progress() {
    if (!this.duration) return 0;
    return (this.currentTime / this.duration) * 100;
  }
}|]

--------------------------------------------------------------------------------
-- Staged Audio Field

renderStagedAudioField :: Field -> Text -> Text -> Lucid.Html ()
renderStagedAudioField field uploadUrl uploadType = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      labelText = fromMaybe name (fcLabel cfg) <> if isReq then " *" else ""
      existingUrl = fromMaybe "" (fcCurrentValue cfg)
      existingUrlJs = escapeJsString existingUrl
      hasExisting = existingUrl /= ""
      inputId = name <> "-input"
      inputIdJs = escapeJsString inputId
      tokenField = name <> "_token"
      maxSizeMB = fromMaybe 500 (fcMaxSizeMB cfg)
      uploadUrlJs = escapeJsString uploadUrl
      uploadTypeJs = escapeJsString uploadType
      playerId = name <> "-player"
      playerIdJs = escapeJsString playerId

  -- Alpine.js state for this upload (includes player state)
  Lucid.div_
    [ xData_
        [i|{
  // Upload state
  uploadToken: '',
  isUploading: false,
  uploadProgress: 0,
  fileName: '',
  fileSize: 0,
  mimeType: '',
  uploadError: '',
  currentCleared: false,

  // Player state (matching Component.AudioPlayer.Waveform)
  playerId: '#{playerIdJs}',
  isPlaying: false,
  originalUrl: '#{existingUrlJs}',
  audioUrl: '#{existingUrlJs}',
  currentTime: 0,
  duration: 0,
  blobUrl: null,

  init() {
    // Initialize global staged upload counter
    window.stagedUploadsInProgress = window.stagedUploadsInProgress || 0;

    const audio = this.$refs.audio;
    audio.addEventListener('loadedmetadata', () => {
      this.duration = audio.duration;
    });
    audio.addEventListener('timeupdate', () => {
      this.currentTime = audio.currentTime;
    });
    audio.addEventListener('ended', () => {
      this.isPlaying = false;
    });

    // Listen for clear events
    window.addEventListener('waveform-player-clear', (e) => {
      if (e.detail && e.detail.playerId === this.playerId) {
        this.clearPlayer();
      }
    });

    // Load audio metadata if we have a URL
    if (this.audioUrl) {
      audio.src = this.audioUrl;
    }
  },

  startUploadTracking() {
    window.stagedUploadsInProgress = (window.stagedUploadsInProgress || 0) + 1;
    window.dispatchEvent(new CustomEvent('staged-upload-change'));
  },

  endUploadTracking() {
    window.stagedUploadsInProgress = Math.max(0, (window.stagedUploadsInProgress || 0) - 1);
    window.dispatchEvent(new CustomEvent('staged-upload-change'));
  },

  async handleFileSelect(event) {
    const file = event.target.files?.[0];
    console.log('[Upload] File selected:', file ? {name: file.name, size: file.size, type: file.type} : 'none');
    if (!file) return;

    if (file.size > #{maxSizeMB * 1024 * 1024}) {
      console.log('[Upload] File too large:', file.size, '> limit:', #{maxSizeMB * 1024 * 1024});
      this.uploadError = 'File exceeds #{maxSizeMB}MB limit';
      return;
    }

    if (!file.type.startsWith('audio/')) {
      console.log('[Upload] Invalid file type:', file.type);
      this.uploadError = 'Please select an audio file';
      return;
    }

    this.uploadError = '';
    this.isUploading = true;
    this.uploadProgress = 0;
    this.fileName = file.name;
    this.fileSize = file.size;
    this.startUploadTracking();

    // Set up player preview with blob URL
    if (this.blobUrl) {
      URL.revokeObjectURL(this.blobUrl);
    }
    this.blobUrl = URL.createObjectURL(file);
    this.audioUrl = this.blobUrl;
    this.pause();
    this.$refs.audio.src = this.audioUrl;
    this.currentTime = 0;
    this.duration = 0;

    const formData = new FormData();
    formData.append('file', file);
    formData.append('upload_type', '#{uploadTypeJs}');

    const xhr = new XMLHttpRequest();
    xhr.upload.addEventListener('progress', (e) => {
      if (e.lengthComputable) {
        this.uploadProgress = Math.round((e.loaded / e.total) * 100);
        console.log('[Upload] Progress:', this.uploadProgress + '%');
      }
    });

    xhr.addEventListener('load', () => {
      console.log('[Upload] Load event - status:', xhr.status, 'statusText:', xhr.statusText);
      console.log('[Upload] Response:', xhr.responseText.substring(0, 500));
      this.isUploading = false;
      this.endUploadTracking();
      if (xhr.status === 200) {
        try {
          const response = JSON.parse(xhr.responseText);
          if (response.success) {
            console.log('[Upload] Success - token:', response.data.token);
            this.uploadToken = response.data.token;
            this.mimeType = response.data.mimeType;
            this.currentCleared = false;
            this.uploadError = '';
          } else {
            console.log('[Upload] Server returned error:', response.error);
            this.uploadError = response.error || 'Upload failed';
            this.clearUpload();
          }
        } catch (e) {
          console.log('[Upload] JSON parse error:', e);
          this.uploadError = 'Invalid server response';
          this.clearUpload();
        }
      } else {
        console.log('[Upload] Non-200 status:', xhr.status, xhr.statusText);
        this.uploadError = 'Upload failed: ' + xhr.statusText;
        this.clearUpload();
      }
    });

    xhr.addEventListener('error', (e) => {
      console.log('[Upload] XHR error event:', e);
      this.isUploading = false;
      this.endUploadTracking();
      this.uploadError = 'Network error during upload';
      this.clearUpload();
    });

    xhr.addEventListener('abort', () => {
      console.log('[Upload] XHR aborted');
      this.isUploading = false;
      this.endUploadTracking();
      this.uploadError = 'Upload aborted';
      this.clearUpload();
    });

    xhr.addEventListener('timeout', () => {
      console.log('[Upload] XHR timeout');
      this.isUploading = false;
      this.endUploadTracking();
      this.uploadError = 'Upload timed out';
      this.clearUpload();
    });

    xhr.addEventListener('readystatechange', () => {
      console.log('[Upload] ReadyState:', xhr.readyState, '| Status:', xhr.status);
    });

    console.log('[Upload] Starting XHR to:', '#{uploadUrlJs}');
    xhr.open('POST', '#{uploadUrlJs}');
    // Send cookies cross-origin
    xhr.withCredentials = true;
    xhr.send(formData);
    console.log('[Upload] XHR sent');
  },

  clearUpload() {
    const input = document.getElementById('#{inputIdJs}');
    if (input) input.value = '';
    this.clearPlayer();
    this.uploadToken = '';
    this.fileName = '';
    this.fileSize = 0;
    this.mimeType = '';
    this.uploadProgress = 0;
    this.currentCleared = true;
  },

  clearPlayer() {
    this.pause();
    if (this.blobUrl) {
      URL.revokeObjectURL(this.blobUrl);
      this.blobUrl = null;
    }
    this.audioUrl = '';
    this.originalUrl = '';
    const audio = this.$refs.audio;
    audio.removeAttribute('src');
    audio.load();
    this.currentTime = 0;
    this.duration = 0;
  },

  // Player methods (matching Component.AudioPlayer.Waveform)
  toggle() {
    this.isPlaying ? this.pause() : this.play();
  },
  play() {
    if (typeof pauseOtherPlayers !== 'undefined') {
      pauseOtherPlayers(this.playerId);
    }
    const audio = this.$refs.audio;
    audio.play().then(() => { this.isPlaying = true; });
  },
  pause() {
    const audio = this.$refs.audio;
    audio.pause();
    this.isPlaying = false;
  },
  seek(event) {
    const bar = event.currentTarget;
    const rect = bar.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const percent = x / rect.width;
    const audio = this.$refs.audio;
    if (this.duration) {
      audio.currentTime = percent * this.duration;
    }
  },
  formatTime(seconds) {
    if (!seconds || isNaN(seconds)) return '0:00';
    const hours = Math.floor(seconds / 3600);
    const mins = Math.floor((seconds % 3600) / 60);
    const secs = Math.floor(seconds % 60);
    if (hours > 0) {
      return hours + ':' + (mins < 10 ? '0' : '') + mins + ':' + (secs < 10 ? '0' : '') + secs;
    }
    return mins + ':' + (secs < 10 ? '0' : '') + secs;
  },
  formatFileSize(bytes) {
    if (bytes === 0) return '0 B';
    const k = 1024;
    const sizes = ['B', 'KB', 'MB', 'GB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return Math.round(bytes / Math.pow(k, i) * 10) / 10 + ' ' + sizes[i];
  },
  triggerInput() {
    document.getElementById('#{inputIdJs}').click();
  },
  get progress() {
    if (!this.duration) return 0;
    return (this.currentTime / this.duration) * 100;
  },
  get isProcessing() {
    return this.uploadProgress >= 100 && this.isUploading && !this.uploadToken;
  },
  get uploadStatusText() {
    if (this.isProcessing) return 'Processing...';
    return this.uploadProgress + '%';
  }
}|],
      Lucid.class_ "fb-field"
    ]
    $ do
      -- Label
      Lucid.label_
        [ Lucid.for_ inputId,
          Lucid.class_ "fb-label"
        ]
        (Lucid.toHtml labelText)

      -- Hidden token input
      Lucid.input_
        [ Lucid.type_ "hidden",
          Lucid.name_ tokenField,
          xModel_ "uploadToken"
        ]

      -- Hidden clear flag
      Lucid.input_
        [ Lucid.type_ "hidden",
          Lucid.name_ (name <> "_clear"),
          xBindValue_ "currentCleared ? 'true' : ''"
        ]

      -- Audio container
      Lucid.div_ [Lucid.class_ "fb-audio-container"] $ do
        -- Hidden file input
        Lucid.input_
          [ Lucid.type_ "file",
            Lucid.id_ inputId,
            Lucid.accept_ "audio/mpeg,.mp3",
            Lucid.class_ "fb-file-input",
            xOnChange_ "handleFileSelect($event)"
          ]

        -- Audio player (always visible, matching Component.AudioPlayer.Waveform)
        Lucid.div_ [Lucid.class_ "fb-player"] $ do
          Lucid.audio_ [xRef_ "audio", Lucid.preload_ "metadata"] mempty

          -- Player controls (always visible)
          Lucid.div_ [Lucid.class_ "fb-player-controls"] $ do
            Lucid.button_
              [ Lucid.type_ "button",
                Lucid.class_ "fb-player-button",
                xOnClick_ "toggle()",
                xText_ "isPlaying ? 'PAUSE' : 'PLAY'"
              ]
              "PLAY"

            Lucid.div_ [Lucid.class_ "fb-player-progress", xOnClick_ "seek($event)"] $
              Lucid.div_ [Lucid.class_ "fb-player-progress-fill", xBindStyle_ "{ width: progress + '%' }"] mempty

            Lucid.span_ [Lucid.class_ "fb-player-time", xText_ "formatTime(currentTime) + ' / ' + formatTime(duration)"] "0:00 / 0:00"

        -- Upload progress bar
        Lucid.div_
          [ xShow_ "isUploading",
            Lucid.class_ "fb-progress",
            Lucid.style_ "display: none;"
          ]
          $ do
            Lucid.div_ [Lucid.class_ "fb-progress-header"] $ do
              Lucid.span_ [xText_ "fileName"] ""
              Lucid.span_ [Lucid.class_ "fb-progress-text", xText_ "uploadStatusText"] ""
            Lucid.div_ [Lucid.class_ "fb-progress-track"] $
              Lucid.div_
                [ Lucid.class_ "fb-progress-bar",
                  xBindStyle_ "{ width: uploadProgress + '%' }",
                  xBindClass_ "{ 'fb-progress-bar--processing': isProcessing }"
                ]
                mempty

        -- Current/existing file state (shows when we have existing audio but no new upload)
        when hasExisting
          $ Lucid.div_
            [ xShow_ "!uploadToken && !currentCleared",
              Lucid.class_ "fb-audio-current"
            ]
          $ do
            Lucid.div_ [Lucid.class_ "fb-audio-current-info"] $ do
              Lucid.span_ [Lucid.class_ "fb-audio-current-icon"] "✓"
              Lucid.span_ [Lucid.class_ "fb-audio-current-label"] "Current file uploaded"
            Lucid.div_ [Lucid.class_ "fb-audio-current-actions"] $ do
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "triggerInput()",
                  Lucid.class_ "fb-audio-button"
                ]
                "REPLACE FILE"
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "clearUpload()",
                  Lucid.class_ "fb-audio-remove"
                ]
                "Remove"

        -- Empty state
        let emptyCondition =
              if hasExisting
                then "!uploadToken && currentCleared && !isUploading"
                else "!uploadToken && !isUploading"
        Lucid.div_
          [ xShow_ emptyCondition,
            Lucid.class_ "fb-audio-empty",
            Lucid.style_ (if hasExisting then "display: none;" else "")
          ]
          $ do
            Lucid.button_
              [ Lucid.type_ "button",
                xOnClick_ "triggerInput()",
                Lucid.class_ "fb-audio-button"
              ]
              "CHOOSE AUDIO FILE"
            Lucid.p_ [Lucid.class_ "fb-hint"] $
              Lucid.toHtml ([i|MP3 only · Max #{maxSizeMB}MB|] :: Text)

        -- Uploaded file display
        Lucid.div_
          [ xShow_ "uploadToken && !isUploading",
            Lucid.class_ "fb-audio-uploaded",
            Lucid.style_ "display: none;"
          ]
          $ do
            Lucid.div_ [Lucid.class_ "fb-audio-uploaded-info"] $ do
              Lucid.span_ [Lucid.class_ "fb-audio-uploaded-icon"] "✓"
              Lucid.span_ [Lucid.class_ "fb-audio-uploaded-name", xText_ "fileName"] ""
              Lucid.span_ [Lucid.class_ "fb-audio-uploaded-size", xText_ "formatFileSize(fileSize)"] ""
            Lucid.div_ [Lucid.class_ "fb-audio-uploaded-actions"] $ do
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "triggerInput()",
                  Lucid.class_ "fb-audio-button"
                ]
                "CHANGE FILE"
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "clearUpload()",
                  Lucid.class_ "fb-audio-remove"
                ]
                "Remove"

        -- Error message
        Lucid.span_
          [ xShow_ "uploadError",
            Lucid.class_ "fb-error",
            Lucid.style_ "display: none;"
          ]
          $ do
            Lucid.span_ [xText_ "uploadError"] ""

--------------------------------------------------------------------------------
-- Staged Image Field

renderStagedImageField :: Field -> Text -> Text -> Lucid.Html ()
renderStagedImageField field uploadUrl uploadType = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      labelText = fromMaybe name (fcLabel cfg) <> if isReq then " *" else ""
      existingUrl = fromMaybe "" (fcCurrentValue cfg)
      hasExisting = existingUrl /= ""
      inputId = name <> "-input"
      inputIdJs = escapeJsString inputId
      tokenField = name <> "_token"
      maxSizeMB = fromMaybe 10 (fcMaxSizeMB cfg)
      uploadUrlJs = escapeJsString uploadUrl
      uploadTypeJs = escapeJsString uploadType
      (ratioW, ratioH) = fromMaybe (1, 1) (fcAspectRatio cfg)
      aspectRatioStyle :: Text
      aspectRatioStyle = [i|aspect-ratio: #{ratioW} / #{ratioH}|]
      ratioText :: Text
      ratioText = [i|#{ratioW}:#{ratioH}|]

  -- Alpine.js state for this upload
  Lucid.div_
    [ xData_
        [i|{
  uploadToken: '',
  isUploading: false,
  uploadProgress: 0,
  fileName: '',
  fileSize: 0,
  mimeType: '',
  previewUrl: '',
  uploadError: '',
  currentCleared: false,

  init() {
    window.stagedUploadsInProgress = window.stagedUploadsInProgress || 0;
  },

  startUploadTracking() {
    window.stagedUploadsInProgress = (window.stagedUploadsInProgress || 0) + 1;
    window.dispatchEvent(new CustomEvent('staged-upload-change'));
  },

  endUploadTracking() {
    window.stagedUploadsInProgress = Math.max(0, (window.stagedUploadsInProgress || 0) - 1);
    window.dispatchEvent(new CustomEvent('staged-upload-change'));
  },

  get isProcessing() {
    return this.uploadProgress >= 100 && this.isUploading && !this.uploadToken;
  },

  get uploadStatusText() {
    if (this.isProcessing) return 'Processing...';
    return this.uploadProgress + '%';
  },

  async handleFileSelect(event) {
    const file = event.target.files?.[0];
    if (!file) return;

    if (file.size > #{maxSizeMB * 1024 * 1024}) {
      this.uploadError = 'File exceeds #{maxSizeMB}MB limit';
      return;
    }

    if (!file.type.startsWith('image/')) {
      this.uploadError = 'Please select an image file';
      return;
    }

    if (this.previewUrl) URL.revokeObjectURL(this.previewUrl);
    this.previewUrl = URL.createObjectURL(file);

    this.uploadError = '';
    this.isUploading = true;
    this.uploadProgress = 0;
    this.fileName = file.name;
    this.fileSize = file.size;
    this.startUploadTracking();

    const formData = new FormData();
    formData.append('file', file);
    formData.append('upload_type', '#{uploadTypeJs}');

    const xhr = new XMLHttpRequest();
    xhr.upload.addEventListener('progress', (e) => {
      if (e.lengthComputable) {
        this.uploadProgress = Math.round((e.loaded / e.total) * 100);
      }
    });

    xhr.addEventListener('load', () => {
      this.isUploading = false;
      this.endUploadTracking();
      if (xhr.status === 200) {
        try {
          const response = JSON.parse(xhr.responseText);
          if (response.success) {
            this.uploadToken = response.data.token;
            this.mimeType = response.data.mimeType;
            this.currentCleared = false;
            this.uploadError = '';
          } else {
            this.uploadError = response.error || 'Upload failed';
            this.clearUpload();
          }
        } catch (e) {
          this.uploadError = 'Invalid server response';
          this.clearUpload();
        }
      } else {
        this.uploadError = 'Upload failed: ' + xhr.statusText;
        this.clearUpload();
      }
    });

    xhr.addEventListener('error', () => {
      this.isUploading = false;
      this.endUploadTracking();
      this.uploadError = 'Network error during upload';
      this.clearUpload();
    });

    xhr.open('POST', '#{uploadUrlJs}');
    xhr.send(formData);
  },

  clearUpload() {
    const input = document.getElementById('#{inputIdJs}');
    if (input) input.value = '';
    if (this.previewUrl) URL.revokeObjectURL(this.previewUrl);
    this.uploadToken = '';
    this.fileName = '';
    this.fileSize = 0;
    this.mimeType = '';
    this.previewUrl = '';
    this.uploadProgress = 0;
    this.currentCleared = true;
  },

  formatFileSize(bytes) {
    if (bytes === 0) return '0 B';
    const k = 1024;
    const sizes = ['B', 'KB', 'MB', 'GB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return Math.round(bytes / Math.pow(k, i) * 10) / 10 + ' ' + sizes[i];
  },

  triggerInput() {
    document.getElementById('#{inputIdJs}').click();
  }
}|],
      Lucid.class_ "fb-field"
    ]
    $ do
      -- Label
      Lucid.label_
        [ Lucid.for_ inputId,
          Lucid.class_ "fb-label"
        ]
        (Lucid.toHtml labelText)

      -- Hidden token input
      Lucid.input_
        [ Lucid.type_ "hidden",
          Lucid.name_ tokenField,
          xModel_ "uploadToken"
        ]

      -- Hidden clear flag
      Lucid.input_
        [ Lucid.type_ "hidden",
          Lucid.name_ (name <> "_clear"),
          xBindValue_ "currentCleared ? 'true' : ''"
        ]

      -- Hidden file input
      Lucid.input_ $
        [ Lucid.type_ "file",
          Lucid.name_ name,
          Lucid.id_ inputId,
          Lucid.accept_ "image/jpeg,image/jpg,image/png,image/webp,image/gif",
          Lucid.class_ "fb-file-input",
          xOnChange_ "handleFileSelect($event)"
        ]
          <> [Lucid.required_ "" | isReq && not hasExisting]

      -- Drop zone container
      Lucid.div_
        [ Lucid.class_ "fb-image-zone",
          xBindClass_ [i|{ 'fb-image-zone--error': uploadError }|],
          xOnClick_ "triggerInput()"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "fb-image-content"] $ do
            -- Upload progress
            Lucid.div_
              [ xShow_ "isUploading",
                Lucid.class_ "fb-progress",
                Lucid.style_ "display: none;"
              ]
              $ do
                Lucid.div_ [Lucid.class_ "fb-progress-header"] $ do
                  Lucid.span_ [xText_ "fileName"] ""
                  Lucid.span_ [Lucid.class_ "fb-progress-text", xText_ "uploadStatusText"] ""
                Lucid.div_ [Lucid.class_ "fb-progress-track"] $
                  Lucid.div_
                    [ Lucid.class_ "fb-progress-bar",
                      xBindStyle_ "{ width: uploadProgress + '%' }",
                      xBindClass_ "{ 'fb-progress-bar--processing': isProcessing }"
                    ]
                    mempty

            -- Empty state
            let emptyCondition =
                  if hasExisting
                    then "!uploadToken && !previewUrl && currentCleared && !isUploading"
                    else "!uploadToken && !previewUrl && !isUploading"

            Lucid.div_
              [ xShow_ emptyCondition,
                Lucid.style_ (if hasExisting then "display: none;" else ""),
                Lucid.class_ "fb-image-empty"
              ]
              $ do
                Lucid.div_
                  [ Lucid.class_ "fb-image-placeholder",
                    Lucid.style_ aspectRatioStyle
                  ]
                  $ do
                    Lucid.div_ [Lucid.class_ "fb-image-icon"] $
                      Lucid.toHtmlRaw ("&#x1F5BC;" :: Text)
                Lucid.p_ [Lucid.class_ "fb-image-cta-text"] "Click to upload image"
                Lucid.p_ [Lucid.class_ "fb-image-meta"] $ do
                  Lucid.toHtml ratioText
                  Lucid.span_ [Lucid.class_ "fb-image-meta-sep"] " · "
                  Lucid.toHtml ([i|Max #{maxSizeMB}MB|] :: Text)

            -- Current/existing image
            when hasExisting
              $ Lucid.div_
                [ xShow_ "!uploadToken && !previewUrl && !currentCleared && !isUploading",
                  Lucid.class_ "fb-image-current"
                ]
              $ do
                Lucid.div_
                  [ Lucid.class_ "fb-image-preview",
                    Lucid.style_ aspectRatioStyle
                  ]
                  $ do
                    Lucid.img_
                      [ Lucid.src_ existingUrl,
                        Lucid.class_ "fb-image-img",
                        Lucid.alt_ "Current image"
                      ]
                Lucid.p_ [Lucid.class_ "fb-image-current-label"] "Current image"
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "$event.stopPropagation(); clearUpload()",
                    Lucid.class_ "fb-image-remove"
                  ]
                  "Remove image"

            -- New file preview
            Lucid.div_
              [ xShow_ "(uploadToken || previewUrl) && !isUploading",
                Lucid.style_ "display: none;",
                Lucid.class_ "fb-image-new"
              ]
              $ do
                Lucid.div_
                  [ Lucid.class_ "fb-image-preview fb-image-preview--new",
                    Lucid.style_ aspectRatioStyle
                  ]
                  $ do
                    Lucid.img_
                      [ xBindSrc_ "previewUrl",
                        Lucid.class_ "fb-image-img",
                        Lucid.alt_ "Preview"
                      ]
                Lucid.div_ [Lucid.class_ "fb-image-info"] $ do
                  Lucid.span_ [Lucid.class_ "fb-image-uploaded-icon"] "✓"
                  Lucid.span_ [Lucid.class_ "fb-image-filename", xText_ "fileName"] ""
                  Lucid.span_ [Lucid.class_ "fb-image-filesize", xText_ "formatFileSize(fileSize)"] ""
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "$event.stopPropagation(); clearUpload()",
                    Lucid.class_ "fb-image-remove"
                  ]
                  "Remove"

      -- Error message
      Lucid.span_
        [ xShow_ "uploadError",
          Lucid.class_ "fb-error",
          Lucid.style_ "display: none;"
        ]
        $ do
          Lucid.span_ [xText_ "uploadError"] ""

      -- Hint for non-required with existing
      when (not isReq && hasExisting) $
        Lucid.p_
          [Lucid.class_ "fb-hint"]
          "Leave unchanged to keep the current image"

--------------------------------------------------------------------------------
-- DateTime Field

renderDateTimeField :: Field -> Lucid.Html ()
renderDateTimeField field = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      hasVal = needsValidation field
      capName = capitalizeFirst name

  Lucid.div_ [Lucid.class_ "fb-field"] $ do
    -- Label
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ "fb-label"]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq then " *" else "")

    -- DateTime input
    Lucid.input_ $
      [ Lucid.type_ "datetime-local",
        Lucid.name_ name,
        Lucid.id_ name,
        Lucid.class_ "fb-input"
      ]
        <> maybe [] (\v -> [Lucid.value_ v]) (fcInitialValue cfg)
        <> [Lucid.required_ "" | isReq]
        <> [Lucid.disabled_ "" | fcDisabled cfg]
        <> if hasVal
          then
            [ xModel_ ("fields." <> name <> ".value"),
              xBindClass_ ("{ 'fb-input--error': showErrors && !fields." <> name <> ".isValid }"),
              xOnChange_ ("showErrors && validate" <> capName <> "()")
            ]
          else []

    -- Error message
    when hasVal $
      Lucid.span_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ "fb-error",
          Lucid.style_ "display: none;"
        ]
        "Please select a date and time"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ "fb-hint"] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- Number Field

renderNumberField :: Field -> Maybe Int -> Maybe Int -> Maybe Int -> Lucid.Html ()
renderNumberField field minVal maxVal step = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      hasVal = needsValidation field
      capName = capitalizeFirst name

  Lucid.div_ [Lucid.class_ "fb-field"] $ do
    -- Label
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ "fb-label"]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq then " *" else "")

    -- Number input
    Lucid.input_ $
      [ Lucid.type_ "number",
        Lucid.name_ name,
        Lucid.id_ name,
        Lucid.class_ "fb-input"
      ]
        <> maybe [] (\v -> [Lucid.value_ v]) (fcInitialValue cfg)
        <> maybe [] (\n -> [Lucid.min_ (Text.pack $ show n)]) minVal
        <> maybe [] (\n -> [Lucid.max_ (Text.pack $ show n)]) maxVal
        <> maybe [] (\n -> [Lucid.step_ (Text.pack $ show n)]) step
        <> [Lucid.required_ "" | isReq]
        <> [Lucid.disabled_ "" | fcDisabled cfg]
        <> if hasVal
          then
            [ xModel_ ("fields." <> name <> ".value"),
              xBindClass_ ("{ 'fb-input--error': showErrors && !fields." <> name <> ".isValid }"),
              xOnInput_ ("showErrors && validate" <> capName <> "()")
            ]
          else []

    -- Error message
    when hasVal $
      Lucid.span_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ "fb-error",
          Lucid.style_ "display: none;"
        ]
        "Please enter a valid number"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ "fb-hint"] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- Checkbox Field

renderCheckboxField :: Field -> Lucid.Html ()
renderCheckboxField field = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      hasVal = needsValidation field
      capName = capitalizeFirst name
      hasDescription = case fcDescriptionHtml cfg of
        Just _ -> True
        Nothing -> False

  Lucid.div_ [Lucid.class_ "fb-field fb-checkbox"] $ do
    Lucid.div_ [Lucid.class_ "fb-checkbox-wrapper"] $ do
      -- Checkbox input
      Lucid.input_ $
        [ Lucid.type_ "checkbox",
          Lucid.name_ name,
          Lucid.id_ name,
          Lucid.class_ "fb-checkbox-input",
          Lucid.value_ "on"
        ]
          <> [Lucid.required_ "" | isReq]
          <> [Lucid.disabled_ "" | fcDisabled cfg]
          <> [Lucid.checked_ | fcChecked cfg]
          <> if hasVal
            then
              [ xModel_ ("fields." <> name <> ".value"),
                xOnChange_ ("showErrors && validate" <> capName <> "()")
              ]
            else []

      -- Label block
      Lucid.label_ [Lucid.for_ name, Lucid.class_ "fb-checkbox-label"] $ do
        Lucid.span_ [Lucid.class_ "fb-checkbox-label-text"] $ do
          Lucid.toHtml $ fromMaybe name (fcLabel cfg)
          when isReq $ Lucid.span_ [Lucid.class_ "fb-checkbox-required"] "*"

        -- Rich HTML description (if provided)
        forM_ (fcDescriptionHtml cfg) $ \descHtml ->
          Lucid.div_ [Lucid.class_ "fb-checkbox-description"] descHtml

        -- Fallback to hint as description (if no descriptionHtml)
        unless hasDescription $
          forM_ (fcHint cfg) $ \h ->
            Lucid.div_ [Lucid.class_ "fb-checkbox-description"] (Lucid.toHtml h)

    -- Error message
    when hasVal $
      Lucid.span_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ "fb-error",
          Lucid.style_ "display: none;"
        ]
        "This field is required"

--------------------------------------------------------------------------------
-- Toggle Field

renderToggleField :: Field -> Lucid.Html ()
renderToggleField field = do
  let name = fName field
      cfg = fConfig field
      isDisabled = fcDisabled cfg
      isChecked = fcChecked cfg
      offLabelText = fromMaybe "Off" (fcOffLabel cfg)
      onLabelText = fromMaybe "On" (fcOnLabel cfg)
      offVal = escapeJsString $ fromMaybe "off" (fcOffValue cfg)
      onVal = escapeJsString $ fromMaybe "on" (fcOnValue cfg)
      toggleId = name <> "-toggle"
      disabledClass = if isDisabled then " fb-toggle--disabled" else ""

  Lucid.div_ [Lucid.class_ "fb-field"] $ do
    -- Optional main label
    forM_ (fcLabel cfg) $ \lbl ->
      Lucid.label_ [Lucid.class_ "fb-label"] (Lucid.toHtml lbl)

    -- Toggle container with Alpine.js state
    Lucid.div_
      [ xData_ [i|{ isOn: #{if isChecked then "true" else "false" :: Text} }|],
        Lucid.class_ ("fb-toggle" <> disabledClass)
      ]
      $ do
        -- Hidden input that submits the actual value
        Lucid.input_
          [ Lucid.type_ "hidden",
            Lucid.name_ name,
            xBindValue_ [i|isOn ? '#{onVal}' : '#{offVal}'|]
          ]

        -- Off label
        Lucid.span_ [Lucid.class_ "fb-toggle-off-label"] $
          Lucid.toHtml offLabelText

        -- Toggle switch
        Lucid.label_ [Lucid.class_ "fb-toggle-switch"] $ do
          Lucid.input_ $
            [ Lucid.type_ "checkbox",
              Lucid.id_ toggleId,
              Lucid.class_ "fb-toggle-input",
              xModel_ "isOn"
            ]
              <> [Lucid.disabled_ "disabled" | isDisabled]
          Lucid.div_ [Lucid.class_ "fb-toggle-track", xBindClass_ "{ 'fb-toggle--checked': isOn }"] mempty

        -- On label
        Lucid.span_ [Lucid.class_ "fb-toggle-on-label"] $
          Lucid.toHtml onLabelText

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ "fb-hint"] (Lucid.toHtml h)
