{-# LANGUAGE QuasiQuotes #-}

-- | HTML rendering for Form V2.
--
-- This module converts 'FormState' to Lucid HTML, handling:
-- - Alpine.js state generation and binding
-- - Field rendering with validation attributes
-- - Section grouping
-- - HTMX integration
module Component.Form.Builder.Render
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

import Component.AudioPlayer.Waveform qualified as WaveformPlayer
import Component.Form.Builder.Alpine (collectAllFields, generateAlpineState, isFileField, needsValidation)
import Component.Form.Builder.Core (FormBuilder, runFormBuilder)
import Component.Form.Builder.JS (capitalizeFirst)
import Component.Form.Builder.Styles (FormStyles (..), defaultFormStyles)
import Component.Form.Builder.Types
import Control.Monad (forM_, unless, when)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Lucid qualified
import Lucid.Base (makeAttributes)
import Lucid.Extras

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
    fcHtmxSwap :: Maybe Text,
    -- | CSS styles for form elements
    fcStyles :: FormStyles
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
      fcHtmxSwap = Nothing,
      fcStyles = defaultFormStyles
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
      styles = fcStyles config

  -- Render title/subtitle (from builder state, or fallback to config header)
  case (fsTitle state, fsSubtitle state) of
    (Nothing, Nothing) -> fromMaybe mempty (fcHeader config)
    _ -> renderFormHeader styles state

  -- Wrap in Alpine if validation needed
  if hasValidatedFields
    then Lucid.div_ [xData_ alpineState, Lucid.class_ "w-full"] $ do
      renderFormElement config state allFields hasFileFields styles
    else renderFormElement config state allFields hasFileFields styles

-- | Render the form header (title and subtitle).
renderFormHeader :: FormStyles -> FormState -> Lucid.Html ()
renderFormHeader styles state =
  Lucid.header_ [Lucid.class_ "mb-6"] $ do
    forM_ (fsTitle state) $ \t ->
      Lucid.h1_ [Lucid.class_ (fsTitleClasses styles)] (Lucid.toHtml t)
    forM_ (fsSubtitle state) $ \st ->
      Lucid.p_ [Lucid.class_ (fsSubtitleClasses styles)] (Lucid.toHtml st)

-- | Render the form element itself.
renderFormElement :: FormConfig -> FormState -> [Field] -> Bool -> FormStyles -> Lucid.Html ()
renderFormElement config state allFields hasFileFields styles = do
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
        Lucid.class_ "w-full"
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
      Lucid.div_ [Lucid.class_ (fsFormClasses styles)] $ do
        -- Form elements (sections and fields in order)
        forM_ (fsElements state) $ \element ->
          renderElement styles element

        -- Footer items (from builder state, or fallback to config footer)
        case fsFooterItems state of
          [] -> fromMaybe mempty (fcFooter config)
          items -> renderFooter styles items (fsFooterHint state)

--------------------------------------------------------------------------------
-- Element Rendering

-- | Render a form element (section or field).
renderElement :: FormStyles -> FormElement -> Lucid.Html ()
renderElement styles (SectionElement sec) = renderSection styles sec
renderElement styles (FieldElement field) = renderField styles field

--------------------------------------------------------------------------------
-- Section Rendering

-- | Render a form section.
renderSection :: FormStyles -> Section -> Lucid.Html ()
renderSection styles sec = do
  let shouldRender = fromMaybe True (secCondition sec)
  when shouldRender $ do
    Lucid.section_ [Lucid.class_ (fsSectionContainerClasses styles)] $ do
      Lucid.h2_ [Lucid.class_ (fsSectionTitleClasses styles)] $
        Lucid.toHtml (secTitle sec)
      Lucid.div_ [Lucid.class_ (fsSectionContentClasses styles)] $
        forM_ (secFields sec) $ \field ->
          renderField styles field

--------------------------------------------------------------------------------
-- Footer Rendering

-- | Render form footer (buttons and inline controls like toggles).
renderFooter :: FormStyles -> [FormFooterItem] -> Maybe Text -> Lucid.Html ()
renderFooter styles items mHint =
  Lucid.div_ [Lucid.class_ "pt-4"] $ do
    -- Footer hint (displayed above the buttons, right-aligned)
    forM_ mHint $ \hint ->
      Lucid.p_ [Lucid.class_ (fsHintClasses styles <> " text-right mb-2")] (Lucid.toHtml hint)

    -- Upload progress bar (shown during file uploads)
    Lucid.div_
      [ xShow_ "isUploading",
        Lucid.class_ "mb-4"
      ]
      $ do
        Lucid.div_ [Lucid.class_ "flex justify-between text-sm text-gray-600 mb-1"] $ do
          Lucid.span_ [] "Uploading..."
          Lucid.span_ [xText_ "uploadProgress + '%'"] ""
        Lucid.div_ [Lucid.class_ "w-full bg-gray-200 border-2 border-gray-800 h-4"] $
          Lucid.div_
            [ Lucid.class_ "bg-black h-full transition-all duration-150",
              xBindStyle_ "{ width: uploadProgress + '%' }"
            ]
            mempty

    Lucid.div_ [Lucid.class_ (fsButtonContainerClasses styles)] $
      forM_ items $ \case
        FooterSubmit lbl ->
          Lucid.button_
            [ Lucid.type_ "submit",
              Lucid.class_ (fsSubmitButtonClasses styles),
              xBindDisabled_ "isUploading",
              xBindClass_ "isUploading ? 'opacity-50 cursor-not-allowed' : ''"
            ]
            (Lucid.toHtml lbl)
        FooterCancel url lbl ->
          Lucid.a_
            [ Lucid.href_ url,
              hxGet_ url,
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ (fsCancelButtonClasses styles)
            ]
            (Lucid.toHtml lbl)
        FooterToggle field ->
          renderFooterToggle styles field

-- | Render a toggle switch inline with footer buttons.
--
-- This is a compact version of toggle rendering designed to sit alongside
-- submit/cancel buttons (e.g., for publish/draft status).
-- Hints are rendered as tooltips (title attribute) since this is an inline context.
renderFooterToggle :: FormStyles -> Field -> Lucid.Html ()
renderFooterToggle styles field = do
  let name = fName field
      cfg = fConfig field
      isDisabled = fcDisabled cfg
      isChecked = fcChecked cfg
      offLabelText = fromMaybe "Off" (fcOffLabel cfg)
      onLabelText = fromMaybe "On" (fcOnLabel cfg)
      offVal = fromMaybe "off" (fcOffValue cfg)
      onVal = fromMaybe "on" (fcOnValue cfg)
      switchClasses =
        if isDisabled
          then fsToggleSwitchDisabledClasses styles
          else fsToggleSwitchClasses styles
      trackClasses =
        if isDisabled
          then fsToggleTrackDisabledClasses styles
          else fsToggleTrackClasses styles
      toggleId = name <> "-toggle"
      hintAttr = maybe [] (\h -> [Lucid.title_ h]) (fcHint cfg)

  -- Compact inline toggle (no wrapper margin, flows with buttons)
  Lucid.div_
    ( [ xData_ [i|{ isOn: #{if isChecked then "true" else "false" :: Text} }|],
        Lucid.class_ (fsToggleContainerClasses styles)
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
      Lucid.span_ [Lucid.class_ (fsToggleOffLabelClasses styles)] $
        Lucid.toHtml offLabelText

      -- Toggle switch
      Lucid.label_ [Lucid.class_ switchClasses] $ do
        Lucid.input_ $
          [ Lucid.type_ "checkbox",
            Lucid.id_ toggleId,
            Lucid.class_ "sr-only peer",
            xModel_ "isOn"
          ]
            <> [Lucid.disabled_ "disabled" | isDisabled]
        Lucid.div_ [Lucid.class_ trackClasses] mempty

      -- On label
      Lucid.span_ [Lucid.class_ (fsToggleOnLabelClasses styles)] $
        Lucid.toHtml onLabelText

--------------------------------------------------------------------------------
-- Field Rendering

-- | Render a single form field.
renderField :: FormStyles -> Field -> Lucid.Html ()
renderField styles field = case fType field of
  TextField -> renderTextField styles field
  PasswordField -> renderPasswordField styles field
  TextareaField rows -> renderTextareaField styles field rows
  SelectField -> renderSelectField styles field
  RadioField -> renderRadioField styles field
  FileField accept -> renderFileField styles field accept
  ImageField {} -> renderImageField styles field
  AudioField {} -> renderAudioField styles field
  StagedAudioField url uploadType -> renderStagedAudioField styles field url uploadType
  StagedImageField url uploadType -> renderStagedImageField styles field url uploadType
  DateTimeField -> renderDateTimeField styles field
  NumberField minV maxV step -> renderNumberField styles field minV maxV step
  CheckboxField -> renderCheckboxField styles field
  ToggleField -> renderToggleField styles field
  PlainHtmlField html -> html

--------------------------------------------------------------------------------
-- Text Field

renderTextField :: FormStyles -> Field -> Lucid.Html ()
renderTextField styles field = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      isDisabled = fcDisabled cfg
      hasVal = needsValidation field && not isDisabled
      capName = capitalizeFirst name
      inputClasses
        | isDisabled = fsTextInputDisabledClasses styles
        | otherwise = fsTextInputClasses styles

  Lucid.div_ [Lucid.class_ "mb-4"] $ do
    -- Label
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq && not isDisabled then " *" else "")

    -- Input
    Lucid.input_ $
      [ Lucid.type_ "text",
        Lucid.name_ name,
        Lucid.id_ name,
        Lucid.class_ inputClasses
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
              xBindClass_ ("showErrors && !fields." <> name <> ".isValid ? '" <> fsTextInputErrorClasses styles <> "' : '" <> fsTextInputClasses styles <> "'"),
              xOnInput_ ("showErrors && validate" <> capName <> "()"),
              xOnBlur_ ("showErrors && validate" <> capName <> "()")
            ]
          else []

    -- Error message
    when hasVal $
      Lucid.div_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ (fsErrorMessageClasses styles),
          Lucid.style_ "display: none;"
        ]
        "Please check this field"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- Password Field

renderPasswordField :: FormStyles -> Field -> Lucid.Html ()
renderPasswordField styles field = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      isDisabled = fcDisabled cfg
      hasVal = needsValidation field && not isDisabled
      capName = capitalizeFirst name
      inputClasses
        | isDisabled = fsTextInputDisabledClasses styles
        | otherwise = fsTextInputClasses styles

  Lucid.div_ [Lucid.class_ "mb-4"] $ do
    -- Label
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq && not isDisabled then " *" else "")

    -- Input
    Lucid.input_ $
      [ Lucid.type_ "password",
        Lucid.name_ name,
        Lucid.id_ name,
        Lucid.class_ inputClasses
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
              xBindClass_ ("showErrors && !fields." <> name <> ".isValid ? '" <> fsTextInputErrorClasses styles <> "' : '" <> fsTextInputClasses styles <> "'"),
              xOnInput_ ("showErrors && validate" <> capName <> "()"),
              xOnBlur_ ("showErrors && validate" <> capName <> "()")
            ]
          else []

    -- Error message
    when hasVal $
      Lucid.div_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ (fsErrorMessageClasses styles),
          Lucid.style_ "display: none;"
        ]
        "Please check this field"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- Textarea Field

renderTextareaField :: FormStyles -> Field -> Int -> Lucid.Html ()
renderTextareaField styles field rows = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      isDisabled = fcDisabled cfg
      hasVal = needsValidation field && not isDisabled
      capName = capitalizeFirst name
      inputClasses
        | isDisabled = fsTextareaDisabledClasses styles
        | otherwise = fsTextareaClasses styles

  Lucid.div_ [Lucid.class_ "mb-4"] $ do
    -- Label
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq && not isDisabled then " *" else "")

    -- Textarea
    Lucid.textarea_
      ( [ Lucid.name_ name,
          Lucid.id_ name,
          Lucid.rows_ (Text.pack $ show rows),
          Lucid.class_ inputClasses
        ]
          <> maybe [] (\p -> [Lucid.placeholder_ p]) (fcPlaceholder cfg)
          <> [Lucid.required_ "" | isReq && not isDisabled]
          <> maybe [] (\n -> [Lucid.minlength_ (Text.pack $ show n)]) (vcMinLength val)
          <> maybe [] (\n -> [Lucid.maxlength_ (Text.pack $ show n)]) (vcMaxLength val)
          <> [Lucid.disabled_ "" | isDisabled]
          <> if hasVal
            then
              [ xModel_ ("fields." <> name <> ".value"),
                xBindClass_ ("showErrors && !fields." <> name <> ".isValid ? '" <> fsTextareaErrorClasses styles <> "' : '" <> fsTextareaClasses styles <> "'"),
                xOnInput_ ("showErrors && validate" <> capName <> "()"),
                xOnBlur_ ("showErrors && validate" <> capName <> "()")
              ]
            else []
      )
      (Lucid.toHtml $ fromMaybe "" (fcInitialValue cfg))

    -- Error message
    when hasVal $
      Lucid.div_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ (fsErrorMessageClasses styles),
          Lucid.style_ "display: none;"
        ]
        "Please check this field"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- Select Field

renderSelectField :: FormStyles -> Field -> Lucid.Html ()
renderSelectField styles field = do
  let options = fcOptions (fConfig field)
      name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      isDisabled = fcDisabled cfg
      hasVal = needsValidation field && not isDisabled
      capName = capitalizeFirst name
      inputClasses
        | isDisabled = fsSelectDisabledClasses styles
        | otherwise = fsSelectClasses styles

  Lucid.div_ [Lucid.class_ "mb-4"] $ do
    -- Label
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq && not isDisabled then " *" else "")

    -- Select
    Lucid.select_
      ( [ Lucid.name_ name,
          Lucid.id_ name,
          Lucid.class_ inputClasses
        ]
          <> [Lucid.required_ "" | isReq && not isDisabled]
          <> [Lucid.disabled_ "" | isDisabled]
          <> if hasVal
            then
              [ xModel_ ("fields." <> name <> ".value"),
                xBindClass_ ("showErrors && !fields." <> name <> ".isValid ? '" <> fsSelectErrorClasses styles <> "' : '" <> fsSelectClasses styles <> "'"),
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
      Lucid.div_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ (fsErrorMessageClasses styles),
          Lucid.style_ "display: none;"
        ]
        "Please select an option"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- Radio Field

renderRadioField :: FormStyles -> Field -> Lucid.Html ()
renderRadioField styles field = do
  let options = fcOptions (fConfig field)
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      hasVal = needsValidation field
      capName = capitalizeFirst name

  Lucid.div_ [Lucid.class_ "mb-4"] $ do
    -- Label
    Lucid.p_ [Lucid.class_ (fsLabelClasses styles)] $
      Lucid.toHtml $
        fromMaybe name (fcLabel cfg) <> if isReq then " *" else ""

    -- Radio group
    Lucid.div_
      ( [Lucid.class_ (fsRadioGroupClasses styles)]
          <> [xBindClass_ ("showErrors && !fields." <> name <> ".isValid ? '" <> fsRadioGroupErrorClasses styles <> "' : '" <> fsRadioGroupClasses styles <> "'") | hasVal]
      )
      $ do
        forM_ options $ \opt -> do
          let optId = name <> "-" <> soValue opt
          Lucid.label_ [Lucid.for_ optId, Lucid.class_ (fsRadioLabelClasses styles)] $ do
            Lucid.input_
              ( [ Lucid.type_ "radio",
                  Lucid.name_ name,
                  Lucid.id_ optId,
                  Lucid.value_ (soValue opt)
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
            Lucid.span_ (Lucid.toHtml $ soLabel opt)
            forM_ (soDescription opt) $ \desc ->
              Lucid.span_ [Lucid.class_ "text-sm text-gray-500 ml-2"] (Lucid.toHtml desc)

    -- Error message
    when hasVal $
      Lucid.div_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ (fsErrorMessageClasses styles),
          Lucid.style_ "display: none;"
        ]
        "Please select an option"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- File Field

renderFileField :: FormStyles -> Field -> Maybe Text -> Lucid.Html ()
renderFileField styles field accept = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      capName = capitalizeFirst name
      buttonLabel = fromMaybe "SELECT FILE" (fcButtonText cfg)

  Lucid.div_ [Lucid.class_ "mb-4"] $ do
    -- Label
    Lucid.label_
      [Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq then " *" else "")

    -- File upload zone
    Lucid.div_
      [ Lucid.class_ (fsFileUploadClasses styles),
        xBindClass_ ("showErrors && !fields." <> name <> ".isValid ? '" <> fsFileUploadErrorClasses styles <> "' : '" <> fsFileUploadClasses styles <> "'")
      ]
      $ do
        -- Hidden file input
        Lucid.input_
          ( [ Lucid.type_ "file",
              Lucid.name_ name,
              Lucid.id_ (name <> "-input"),
              Lucid.class_ "hidden",
              xOnChange_ ("handle" <> capName <> "Change($event)")
            ]
              <> maybe [] (\a -> [Lucid.accept_ a]) accept
          )

        -- Upload button/zone
        Lucid.div_ [xShow_ ("!fields." <> name <> ".fileName")] $ do
          Lucid.label_
            [ Lucid.for_ (name <> "-input"),
              Lucid.class_ "cursor-pointer inline-block bg-gray-800 text-white font-bold py-2 px-4 hover:bg-gray-700"
            ]
            (Lucid.toHtml buttonLabel)
          forM_ (fcHint cfg) $ \h ->
            Lucid.p_ [Lucid.class_ "mt-2 text-sm text-gray-600"] (Lucid.toHtml h)

        -- Selected file display
        Lucid.div_ [xShow_ ("fields." <> name <> ".fileName"), Lucid.class_ "text-left"] $ do
          -- Image preview
          Lucid.template_ [xIf_ ("fields." <> name <> ".previewUrl")] $
            Lucid.img_
              [ xBindSrc_ ("fields." <> name <> ".previewUrl"),
                Lucid.class_ "max-h-32 mb-2"
              ]
          -- File info
          Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
            Lucid.div_ $ do
              Lucid.span_ [Lucid.class_ "font-mono", xText_ ("fields." <> name <> ".fileName")] ""
              Lucid.span_ [Lucid.class_ "text-sm text-gray-500 ml-2", xText_ ("formatFileSize(fields." <> name <> ".fileSize)")] ""
            -- Clear button
            Lucid.button_
              [ Lucid.type_ "button",
                Lucid.class_ "text-red-600 hover:text-red-800",
                xOnClick_ ("clear" <> capName <> "()")
              ]
              "Remove"

    -- Error message
    Lucid.div_
      [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
        Lucid.class_ (fsErrorMessageClasses styles),
        Lucid.style_ "display: none;",
        xText_ ("fields." <> name <> ".error || 'Please select a file'")
      ]
      ""

--------------------------------------------------------------------------------
-- Image Field

renderImageField :: FormStyles -> Field -> Lucid.Html ()
renderImageField styles field = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      labelText = fromMaybe name (fcLabel cfg) <> if isReq then " *" else ""
      inputId = name <> "-input"
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
    const input = document.getElementById('#{inputId}');
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
    const input = document.getElementById('#{inputId}');
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
    const input = document.getElementById('#{inputId}');
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
    document.getElementById('#{inputId}').click();
  },

  formatFileSize(bytes) {
    if (bytes === 0) return '0 B';
    const k = 1024;
    const sizes = ['B', 'KB', 'MB', 'GB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return Math.round(bytes / Math.pow(k, i) * 10) / 10 + ' ' + sizes[i];
  }
}|]
    ]
    $ do
      -- Label
      Lucid.label_
        [ Lucid.for_ inputId,
          Lucid.class_ (fsLabelClasses styles <> " uppercase tracking-wide")
        ]
        (Lucid.toHtml labelText)

      -- Hidden file input
      Lucid.input_ $
        [ Lucid.type_ "file",
          Lucid.name_ name,
          Lucid.id_ inputId,
          Lucid.accept_ accept,
          Lucid.class_ "hidden",
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
            '#{fsImageDropZoneErrorClasses styles}': !isValid,
            '#{fsImageDropZoneDraggingClasses styles}': isDragging && isValid,
            '#{fsImageDropZoneClasses styles}': !isDragging && isValid
          }|],
          xOnClick_ "triggerInput()",
          xOnDragover_ "isDragging = true",
          xOnDragleave_ "isDragging = false",
          xOnDrop_ "handleDrop($event)"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex flex-col items-center"] $ do
            -- Empty/placeholder state
            let emptyCondition =
                  if hasExisting
                    then "currentCleared && !previewUrl"
                    else "!previewUrl"
                emptyStyle = if hasExisting then "display: none;" else ""

            Lucid.div_ [xShow_ emptyCondition, Lucid.style_ emptyStyle, Lucid.class_ "flex flex-col items-center"] $ do
              -- Aspect ratio preview box
              Lucid.div_
                [ Lucid.class_ "w-40 border-2 border-dashed border-gray-300 bg-white flex items-center justify-center mb-4 relative overflow-hidden",
                  Lucid.style_ aspectRatioStyle
                ]
                $ do
                  -- Decorative corner markers
                  Lucid.div_ [Lucid.class_ "absolute top-0 left-0 w-3 h-3 border-t-2 border-l-2 border-gray-400"] mempty
                  Lucid.div_ [Lucid.class_ "absolute top-0 right-0 w-3 h-3 border-t-2 border-r-2 border-gray-400"] mempty
                  Lucid.div_ [Lucid.class_ "absolute bottom-0 left-0 w-3 h-3 border-b-2 border-l-2 border-gray-400"] mempty
                  Lucid.div_ [Lucid.class_ "absolute bottom-0 right-0 w-3 h-3 border-b-2 border-r-2 border-gray-400"] mempty
                  -- Center icon
                  Lucid.div_ [Lucid.class_ "text-gray-300 text-3xl"] $
                    Lucid.toHtmlRaw ("&#x1F5BC;" :: Text)

              -- Call to action
              Lucid.div_ [Lucid.class_ "text-center"] $ do
                Lucid.p_ [Lucid.class_ "font-bold text-gray-700 mb-1"] $
                  Lucid.span_ [xShow_ "!isDragging"] "Drop image here or click to browse"
                Lucid.p_ [Lucid.class_ "font-bold text-purple-600 mb-1", xShow_ "isDragging", Lucid.style_ "display: none;"] "Drop to upload"
                Lucid.p_ [Lucid.class_ "text-sm text-gray-600"] $ do
                  Lucid.toHtml ratioText
                  Lucid.span_ [Lucid.class_ "mx-2 text-gray-300"] "·"
                  Lucid.toHtml ([i|Max #{maxSizeMB}MB|] :: Text)
                  Lucid.span_ [Lucid.class_ "mx-2 text-gray-300"] "·"
                  Lucid.toHtml friendlyTypes

            -- Current/existing image
            when hasExisting $
              Lucid.div_ [xShow_ "!currentCleared && !previewUrl", Lucid.class_ "flex flex-col items-center"] $ do
                Lucid.div_
                  [ Lucid.class_ ("w-40 mb-3 " <> fsImagePreviewClasses styles),
                    Lucid.style_ aspectRatioStyle
                  ]
                  $ do
                    Lucid.img_
                      [ Lucid.src_ existingUrl,
                        Lucid.class_ "w-full h-full object-cover",
                        Lucid.alt_ "Current image"
                      ]
                    Lucid.div_ [Lucid.class_ "absolute inset-0 bg-black/50 opacity-0 group-hover:opacity-100 transition-opacity flex items-center justify-center"] $
                      Lucid.span_ [Lucid.class_ "text-white text-sm font-bold"] "Click to change"
                Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mb-2"] "Current image"
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "$event.stopPropagation(); clearFile()",
                    Lucid.class_ (fsImageActionButtonClasses styles)
                  ]
                  "Remove image"

            -- New file preview
            Lucid.div_ [xShow_ "previewUrl", Lucid.style_ "display: none;", Lucid.class_ "flex flex-col items-center"] $ do
              Lucid.div_
                [ Lucid.class_ ("w-40 mb-3 border-purple-400 " <> fsImagePreviewClasses styles),
                  Lucid.style_ aspectRatioStyle
                ]
                $ do
                  Lucid.img_
                    [ xBindSrc_ "previewUrl",
                      Lucid.class_ "w-full h-full object-cover",
                      Lucid.alt_ "New image preview"
                    ]
                  Lucid.div_ [Lucid.class_ "absolute inset-0 bg-black/50 opacity-0 group-hover:opacity-100 transition-opacity flex items-center justify-center"] $
                    Lucid.span_ [Lucid.class_ "text-white text-sm font-bold"] "Click to change"

              -- File info chip
              Lucid.div_ [Lucid.class_ "flex items-center gap-2 px-3 py-1.5 bg-purple-100 border border-purple-300 text-purple-800 text-sm mb-2"] $ do
                Lucid.span_ [Lucid.class_ "font-bold", xText_ "fileName"] ""
                Lucid.span_ [Lucid.class_ "text-purple-600"] $ do
                  "("
                  Lucid.span_ [xText_ "formatFileSize(fileSize)"] ""
                  ")"
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "$event.stopPropagation(); clearFile()",
                  Lucid.class_ (fsImageActionButtonClasses styles)
                ]
                "Remove"

      -- Error message
      Lucid.div_
        [ xShow_ "!isValid",
          Lucid.class_ (fsErrorMessageClasses styles <> " mt-2 flex items-center gap-2"),
          Lucid.style_ "display: none;"
        ]
        $ do
          Lucid.span_ [Lucid.class_ "font-bold"] "!"
          Lucid.span_ [xText_ "error"] ""

      -- Hint for non-required with existing
      when (not isReq && hasExisting) $
        Lucid.p_
          [Lucid.class_ (fsHintClasses styles <> " mt-2")]
          "Leave unchanged to keep the current image"

      -- Cropper Modal
      Lucid.div_
        [ xShow_ "showCropper",
          Lucid.style_ "display: none;",
          Lucid.class_ "fixed inset-0 z-50 flex items-center justify-center p-4"
        ]
        $ do
          -- Backdrop
          Lucid.div_
            [ xOnClick_ "cancelCrop()",
              Lucid.class_ "absolute inset-0 bg-black/70"
            ]
            mempty

          -- Modal content
          Lucid.div_ [Lucid.class_ "relative bg-white max-w-2xl w-full max-h-[90vh] flex flex-col shadow-xl"] $ do
            -- Header
            Lucid.div_ [Lucid.class_ "flex items-center justify-between px-4 py-3 border-b border-gray-200"] $ do
              Lucid.h3_ [Lucid.class_ "font-bold text-gray-800"] $ do
                "Crop Image"
                Lucid.span_ [Lucid.class_ "font-normal text-sm text-gray-600 ml-2"] $
                  Lucid.toHtml ([i|(#{ratioText} ratio)|] :: Text)
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "cancelCrop()",
                  Lucid.class_ "text-gray-400 hover:text-gray-600 text-xl font-bold"
                ]
                "×"

            -- Cropper container
            Lucid.div_ [Lucid.class_ "flex-1 overflow-hidden bg-gray-900 min-h-[300px] max-h-[60vh]"] $
              Lucid.img_
                [ xRef_ "cropperImage",
                  xBindSrc_ "cropImageUrl",
                  Lucid.class_ "max-w-full",
                  Lucid.alt_ "Image to crop"
                ]

            -- Footer with actions
            Lucid.div_ [Lucid.class_ "flex items-center justify-between px-4 py-3 border-t border-gray-200 bg-gray-50"] $ do
              -- Zoom controls
              Lucid.div_ [Lucid.class_ "flex items-center gap-2"] $ do
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "cropper?.zoom(-0.1)",
                    Lucid.class_ "px-3 py-1 bg-gray-200 hover:bg-gray-300 text-sm font-bold"
                  ]
                  "−"
                Lucid.span_ [Lucid.class_ "text-sm text-gray-600"] "Zoom"
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "cropper?.zoom(0.1)",
                    Lucid.class_ "px-3 py-1 bg-gray-200 hover:bg-gray-300 text-sm font-bold"
                  ]
                  "+"

              -- Action buttons
              Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "cancelCrop()",
                    Lucid.class_ "px-4 py-2 bg-gray-300 hover:bg-gray-400 font-bold text-sm"
                  ]
                  "Cancel"
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "confirmCrop()",
                    Lucid.class_ "px-4 py-2 bg-purple-600 hover:bg-purple-700 text-white font-bold text-sm"
                  ]
                  "Apply Crop"

--------------------------------------------------------------------------------
-- Audio Field

renderAudioField :: FormStyles -> Field -> Lucid.Html ()
renderAudioField styles field = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      labelText = fromMaybe name (fcLabel cfg) <> if isReq then " *" else ""
      existingUrl = fromMaybe "" (fcCurrentValue cfg)
      inputId = name <> "-input"

  Lucid.div_ $ do
    -- Label
    Lucid.label_
      [ Lucid.for_ inputId,
        Lucid.class_ (fsLabelClasses styles <> " uppercase tracking-wide")
      ]
      (Lucid.toHtml labelText)

    -- Dashed border container with player and file picker
    Lucid.div_ [Lucid.class_ (fsAudioContainerClasses styles)] $ do
      -- Audio player (uses WaveformPlayer)
      Lucid.div_ [Lucid.class_ "mb-4"] $
        WaveformPlayer.render
          WaveformPlayer.Config
            { WaveformPlayer.playerId = name <> "-player",
              WaveformPlayer.audioUrl = existingUrl,
              WaveformPlayer.title = fromMaybe name (fcLabel cfg),
              WaveformPlayer.fileInputId = Just (name <> "-input"),
              WaveformPlayer.containerClasses = Nothing,
              WaveformPlayer.buttonClasses = Nothing,
              WaveformPlayer.progressBarClasses = Nothing,
              WaveformPlayer.progressFillClasses = Nothing,
              WaveformPlayer.timeDisplayClasses = Nothing
            }

      -- Hidden file input
      Lucid.input_ $
        [ Lucid.type_ "file",
          Lucid.name_ name,
          Lucid.id_ inputId,
          Lucid.accept_ "audio/*",
          Lucid.class_ "hidden"
        ]
          <> [Lucid.required_ "" | isReq]

      -- File picker button and info
      Lucid.label_ [Lucid.for_ inputId, Lucid.class_ "cursor-pointer"] $ do
        Lucid.div_
          [Lucid.class_ (fsAudioButtonClasses styles)]
          "CHOOSE AUDIO FILE"
        Lucid.p_ [Lucid.class_ (fsHintClasses styles <> " mt-2")] $
          if isReq
            then "MP3, WAV, FLAC accepted • Max 500MB"
            else "MP3, WAV, FLAC accepted • Max 500MB • Leave empty to keep current file"

--------------------------------------------------------------------------------
-- Staged Audio Field

-- | Render a staged audio upload field with background upload and progress bar.
--
-- When the user selects a file, it uploads immediately via XHR to the upload URL.
-- The returned token is stored in a hidden field and submitted with the form.
renderStagedAudioField :: FormStyles -> Field -> Text -> Text -> Lucid.Html ()
renderStagedAudioField styles field uploadUrl uploadType = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      labelText = fromMaybe name (fcLabel cfg) <> if isReq then " *" else ""
      existingUrl = fromMaybe "" (fcCurrentValue cfg)
      hasExisting = existingUrl /= ""
      inputId = name <> "-input"
      tokenField = name <> "_token"
      maxSizeMB = fromMaybe 500 (fcMaxSizeMB cfg)

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
  uploadError: '',
  currentCleared: false,

  async handleFileSelect(event) {
    const file = event.target.files?.[0];
    if (!file) return;

    // Validate file size
    if (file.size > #{maxSizeMB * 1024 * 1024}) {
      this.uploadError = 'File exceeds #{maxSizeMB}MB limit';
      return;
    }

    // Validate file type
    if (!file.type.startsWith('audio/')) {
      this.uploadError = 'Please select an audio file';
      return;
    }

    this.uploadError = '';
    this.isUploading = true;
    this.uploadProgress = 0;
    this.fileName = file.name;
    this.fileSize = file.size;

    // Create FormData
    const formData = new FormData();
    formData.append('file', file);
    formData.append('upload_type', '#{uploadType}');

    // Upload via XHR for progress tracking
    const xhr = new XMLHttpRequest();
    xhr.upload.addEventListener('progress', (e) => {
      if (e.lengthComputable) {
        this.uploadProgress = Math.round((e.loaded / e.total) * 100);
      }
    });

    xhr.addEventListener('load', () => {
      this.isUploading = false;
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
      this.uploadError = 'Network error during upload';
      this.clearUpload();
    });

    xhr.open('POST', '#{uploadUrl}');
    xhr.send(formData);
  },

  clearUpload() {
    const input = document.getElementById('#{inputId}');
    if (input) input.value = '';
    // Clear the waveform player
    window.dispatchEvent(new CustomEvent('waveform-player-clear', {
      detail: { playerId: '#{name}-player' }
    }));
    this.uploadToken = '';
    this.fileName = '';
    this.fileSize = 0;
    this.mimeType = '';
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
    document.getElementById('#{inputId}').click();
  }
}|]
    ]
    $ do
      -- Label
      Lucid.label_
        [ Lucid.for_ inputId,
          Lucid.class_ (fsLabelClasses styles <> " uppercase tracking-wide")
        ]
        (Lucid.toHtml labelText)

      -- Hidden token input (submitted with form)
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

      -- Dashed border container
      Lucid.div_ [Lucid.class_ (fsAudioContainerClasses styles)] $ do
        -- Hidden file input (no name attribute - file is uploaded via XHR, only token is submitted)
        Lucid.input_
          [ Lucid.type_ "file",
            Lucid.id_ inputId,
            Lucid.accept_ "audio/mpeg,.mp3",
            Lucid.class_ "hidden",
            xOnChange_ "handleFileSelect($event)"
          ]

        -- Audio player (watches file input for preview, shows existing if available)
        Lucid.div_ [Lucid.class_ "mb-4"] $
          WaveformPlayer.render
            WaveformPlayer.Config
              { WaveformPlayer.playerId = name <> "-player",
                WaveformPlayer.audioUrl = existingUrl,
                WaveformPlayer.title = fromMaybe "Audio" (fcLabel cfg),
                WaveformPlayer.fileInputId = Just (name <> "-input"),
                WaveformPlayer.containerClasses = Nothing,
                WaveformPlayer.buttonClasses = Nothing,
                WaveformPlayer.progressBarClasses = Nothing,
                WaveformPlayer.progressFillClasses = Nothing,
                WaveformPlayer.timeDisplayClasses = Nothing
              }

        -- Upload progress bar
        Lucid.div_
          [ xShow_ "isUploading",
            Lucid.class_ "mb-4",
            Lucid.style_ "display: none;"
          ]
          $ do
            Lucid.div_ [Lucid.class_ "flex justify-between text-sm text-gray-600 mb-1"] $ do
              Lucid.span_ [xText_ "fileName"] ""
              Lucid.span_ [xText_ "uploadProgress + '%'"] ""
            Lucid.div_ [Lucid.class_ "w-full bg-gray-200 border-2 border-gray-800 h-4"] $
              Lucid.div_
                [ Lucid.class_ "bg-purple-600 h-full transition-all duration-150",
                  xBindStyle_ "{ width: uploadProgress + '%' }"
                ]
                mempty

        -- Current/existing file state
        when hasExisting
          $ Lucid.div_
            [ xShow_ "!uploadToken && !currentCleared",
              Lucid.class_ "text-center"
            ]
          $ do
            Lucid.div_ [Lucid.class_ "flex items-center justify-center gap-3 mb-3"] $ do
              Lucid.span_ [Lucid.class_ "text-green-600 font-bold"] "✓"
              Lucid.span_ [Lucid.class_ "font-bold"] "Current file uploaded"
            Lucid.div_ [Lucid.class_ "flex justify-center gap-2"] $ do
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "triggerInput()",
                  Lucid.class_ (fsAudioButtonClasses styles)
                ]
                "REPLACE FILE"
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "clearUpload()",
                  Lucid.class_ "text-red-600 hover:text-red-800 font-bold"
                ]
                "Remove"

        -- Empty state (no existing, no new upload)
        let emptyCondition =
              if hasExisting
                then "!uploadToken && currentCleared && !isUploading"
                else "!uploadToken && !isUploading"
        Lucid.div_
          [ xShow_ emptyCondition,
            Lucid.class_ "text-center",
            Lucid.style_ (if hasExisting then "display: none;" else "")
          ]
          $ do
            Lucid.button_
              [ Lucid.type_ "button",
                xOnClick_ "triggerInput()",
                Lucid.class_ (fsAudioButtonClasses styles)
              ]
              "CHOOSE AUDIO FILE"
            Lucid.p_ [Lucid.class_ (fsHintClasses styles <> " mt-2")] $
              Lucid.toHtml ([i|MP3 only • Max #{maxSizeMB}MB|] :: Text)

        -- Uploaded file display
        Lucid.div_
          [ xShow_ "uploadToken && !isUploading",
            Lucid.class_ "text-center",
            Lucid.style_ "display: none;"
          ]
          $ do
            Lucid.div_ [Lucid.class_ "flex items-center justify-center gap-3 mb-3"] $ do
              Lucid.span_ [Lucid.class_ "text-green-600 font-bold"] "✓"
              Lucid.span_ [Lucid.class_ "font-mono", xText_ "fileName"] ""
              Lucid.span_ [Lucid.class_ "text-gray-500", xText_ "formatFileSize(fileSize)"] ""
            Lucid.div_ [Lucid.class_ "flex justify-center gap-2"] $ do
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "triggerInput()",
                  Lucid.class_ (fsAudioButtonClasses styles)
                ]
                "CHANGE FILE"
              Lucid.button_
                [ Lucid.type_ "button",
                  xOnClick_ "clearUpload()",
                  Lucid.class_ "text-red-600 hover:text-red-800 font-bold"
                ]
                "Remove"

        -- Error message
        Lucid.div_
          [ xShow_ "uploadError",
            Lucid.class_ (fsErrorMessageClasses styles <> " mt-2"),
            Lucid.style_ "display: none;"
          ]
          $ do
            Lucid.span_ [Lucid.class_ "font-bold mr-2"] "!"
            Lucid.span_ [xText_ "uploadError"] ""

--------------------------------------------------------------------------------
-- Staged Image Field

-- | Render a staged image upload field with background upload and progress bar.
renderStagedImageField :: FormStyles -> Field -> Text -> Text -> Lucid.Html ()
renderStagedImageField styles field uploadUrl uploadType = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      labelText = fromMaybe name (fcLabel cfg) <> if isReq then " *" else ""
      existingUrl = fromMaybe "" (fcCurrentValue cfg)
      hasExisting = existingUrl /= ""
      inputId = name <> "-input"
      tokenField = name <> "_token"
      maxSizeMB = fromMaybe 10 (fcMaxSizeMB cfg)
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

  async handleFileSelect(event) {
    const file = event.target.files?.[0];
    if (!file) return;

    // Validate file size
    if (file.size > #{maxSizeMB * 1024 * 1024}) {
      this.uploadError = 'File exceeds #{maxSizeMB}MB limit';
      return;
    }

    // Validate file type
    if (!file.type.startsWith('image/')) {
      this.uploadError = 'Please select an image file';
      return;
    }

    // Create preview
    if (this.previewUrl) URL.revokeObjectURL(this.previewUrl);
    this.previewUrl = URL.createObjectURL(file);

    this.uploadError = '';
    this.isUploading = true;
    this.uploadProgress = 0;
    this.fileName = file.name;
    this.fileSize = file.size;

    // Create FormData
    const formData = new FormData();
    formData.append('file', file);
    formData.append('upload_type', '#{uploadType}');

    // Upload via XHR for progress tracking
    const xhr = new XMLHttpRequest();
    xhr.upload.addEventListener('progress', (e) => {
      if (e.lengthComputable) {
        this.uploadProgress = Math.round((e.loaded / e.total) * 100);
      }
    });

    xhr.addEventListener('load', () => {
      this.isUploading = false;
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
      this.uploadError = 'Network error during upload';
      this.clearUpload();
    });

    xhr.open('POST', '#{uploadUrl}');
    xhr.send(formData);
  },

  clearUpload() {
    const input = document.getElementById('#{inputId}');
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
    document.getElementById('#{inputId}').click();
  }
}|]
    ]
    $ do
      -- Label
      Lucid.label_
        [ Lucid.for_ inputId,
          Lucid.class_ (fsLabelClasses styles <> " uppercase tracking-wide")
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
          Lucid.class_ "hidden",
          xOnChange_ "handleFileSelect($event)"
        ]
          <> [Lucid.required_ "" | isReq && not hasExisting]

      -- Drop zone container
      Lucid.div_
        [ Lucid.class_ (fsImageDropZoneClasses styles),
          xBindClass_ [i|{ '#{fsImageDropZoneErrorClasses styles}': uploadError }|],
          xOnClick_ "triggerInput()"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex flex-col items-center"] $ do
            -- Upload progress
            Lucid.div_
              [ xShow_ "isUploading",
                Lucid.class_ "w-full max-w-xs mb-4",
                Lucid.style_ "display: none;"
              ]
              $ do
                Lucid.div_ [Lucid.class_ "flex justify-between text-sm text-gray-600 mb-1"] $ do
                  Lucid.span_ [] "Uploading..."
                  Lucid.span_ [xText_ "uploadProgress + '%'"] ""
                Lucid.div_ [Lucid.class_ "w-full bg-gray-200 border border-gray-400 h-3"] $
                  Lucid.div_
                    [ Lucid.class_ "bg-purple-600 h-full transition-all duration-150",
                      xBindStyle_ "{ width: uploadProgress + '%' }"
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
                Lucid.class_ "flex flex-col items-center"
              ]
              $ do
                Lucid.div_
                  [ Lucid.class_ "w-40 border-2 border-dashed border-gray-300 bg-white flex items-center justify-center mb-4",
                    Lucid.style_ aspectRatioStyle
                  ]
                  $ do
                    Lucid.div_ [Lucid.class_ "text-gray-300 text-3xl"] $
                      Lucid.toHtmlRaw ("&#x1F5BC;" :: Text)
                Lucid.p_ [Lucid.class_ "font-bold text-gray-700 mb-1"] "Click to upload image"
                Lucid.p_ [Lucid.class_ "text-sm text-gray-600"] $ do
                  Lucid.toHtml ratioText
                  Lucid.span_ [Lucid.class_ "mx-2 text-gray-300"] "·"
                  Lucid.toHtml ([i|Max #{maxSizeMB}MB|] :: Text)

            -- Current/existing image
            when hasExisting
              $ Lucid.div_
                [ xShow_ "!uploadToken && !previewUrl && !currentCleared && !isUploading",
                  Lucid.class_ "flex flex-col items-center"
                ]
              $ do
                Lucid.div_
                  [ Lucid.class_ ("w-40 mb-3 " <> fsImagePreviewClasses styles),
                    Lucid.style_ aspectRatioStyle
                  ]
                  $ do
                    Lucid.img_
                      [ Lucid.src_ existingUrl,
                        Lucid.class_ "w-full h-full object-cover",
                        Lucid.alt_ "Current image"
                      ]
                Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mb-2"] "Current image"
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "$event.stopPropagation(); clearUpload()",
                    Lucid.class_ (fsImageActionButtonClasses styles)
                  ]
                  "Remove image"

            -- New file preview
            Lucid.div_
              [ xShow_ "(uploadToken || previewUrl) && !isUploading",
                Lucid.style_ "display: none;",
                Lucid.class_ "flex flex-col items-center"
              ]
              $ do
                Lucid.div_
                  [ Lucid.class_ ("w-40 mb-3 border-purple-400 " <> fsImagePreviewClasses styles),
                    Lucid.style_ aspectRatioStyle
                  ]
                  $ do
                    Lucid.img_
                      [ xBindSrc_ "previewUrl",
                        Lucid.class_ "w-full h-full object-cover",
                        Lucid.alt_ "Preview"
                      ]
                Lucid.div_ [Lucid.class_ "flex items-center gap-2 mb-2"] $ do
                  Lucid.span_ [Lucid.class_ "text-green-600 font-bold"] "✓"
                  Lucid.span_ [Lucid.class_ "font-mono text-sm", xText_ "fileName"] ""
                  Lucid.span_ [Lucid.class_ "text-gray-500 text-sm", xText_ "formatFileSize(fileSize)"] ""
                Lucid.button_
                  [ Lucid.type_ "button",
                    xOnClick_ "$event.stopPropagation(); clearUpload()",
                    Lucid.class_ (fsImageActionButtonClasses styles)
                  ]
                  "Remove"

      -- Error message
      Lucid.div_
        [ xShow_ "uploadError",
          Lucid.class_ (fsErrorMessageClasses styles <> " mt-2 flex items-center gap-2"),
          Lucid.style_ "display: none;"
        ]
        $ do
          Lucid.span_ [Lucid.class_ "font-bold"] "!"
          Lucid.span_ [xText_ "uploadError"] ""

      -- Hint for non-required with existing
      when (not isReq && hasExisting) $
        Lucid.p_
          [Lucid.class_ (fsHintClasses styles <> " mt-2")]
          "Leave unchanged to keep the current image"

--------------------------------------------------------------------------------
-- DateTime Field

renderDateTimeField :: FormStyles -> Field -> Lucid.Html ()
renderDateTimeField styles field = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      hasVal = needsValidation field
      capName = capitalizeFirst name

  Lucid.div_ [Lucid.class_ "mb-4"] $ do
    -- Label
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq then " *" else "")

    -- DateTime input
    Lucid.input_ $
      [ Lucid.type_ "datetime-local",
        Lucid.name_ name,
        Lucid.id_ name,
        Lucid.class_ (fsTextInputClasses styles)
      ]
        <> maybe [] (\v -> [Lucid.value_ v]) (fcInitialValue cfg)
        <> [Lucid.required_ "" | isReq]
        <> [Lucid.disabled_ "" | fcDisabled cfg]
        <> if hasVal
          then
            [ xModel_ ("fields." <> name <> ".value"),
              xBindClass_ ("showErrors && !fields." <> name <> ".isValid ? '" <> fsTextInputErrorClasses styles <> "' : '" <> fsTextInputClasses styles <> "'"),
              xOnChange_ ("showErrors && validate" <> capName <> "()")
            ]
          else []

    -- Error message
    when hasVal $
      Lucid.div_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ (fsErrorMessageClasses styles),
          Lucid.style_ "display: none;"
        ]
        "Please select a date and time"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- Number Field

renderNumberField :: FormStyles -> Field -> Maybe Int -> Maybe Int -> Maybe Int -> Lucid.Html ()
renderNumberField styles field minVal maxVal step = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      hasVal = needsValidation field
      capName = capitalizeFirst name

  Lucid.div_ [Lucid.class_ "mb-4"] $ do
    -- Label
    Lucid.label_
      [Lucid.for_ name, Lucid.class_ (fsLabelClasses styles)]
      (Lucid.toHtml $ fromMaybe name (fcLabel cfg) <> if isReq then " *" else "")

    -- Number input
    Lucid.input_ $
      [ Lucid.type_ "number",
        Lucid.name_ name,
        Lucid.id_ name,
        Lucid.class_ (fsTextInputClasses styles)
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
              xBindClass_ ("showErrors && !fields." <> name <> ".isValid ? '" <> fsTextInputErrorClasses styles <> "' : '" <> fsTextInputClasses styles <> "'"),
              xOnInput_ ("showErrors && validate" <> capName <> "()")
            ]
          else []

    -- Error message
    when hasVal $
      Lucid.div_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ (fsErrorMessageClasses styles),
          Lucid.style_ "display: none;"
        ]
        "Please enter a valid number"

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)

--------------------------------------------------------------------------------
-- Checkbox Field

renderCheckboxField :: FormStyles -> Field -> Lucid.Html ()
renderCheckboxField styles field = do
  let name = fName field
      cfg = fConfig field
      val = fValidation field
      isReq = vcRequired val
      hasVal = needsValidation field
      capName = capitalizeFirst name
      hasDescription = case fcDescriptionHtml cfg of
        Just _ -> True
        Nothing -> False

  Lucid.div_ [Lucid.class_ "mb-4"] $ do
    Lucid.div_ [Lucid.class_ "flex items-start"] $ do
      -- Checkbox input
      Lucid.input_ $
        [ Lucid.type_ "checkbox",
          Lucid.name_ name,
          Lucid.id_ name,
          Lucid.class_ "mr-3 mt-1",
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

      -- Label block (bold label + optional description)
      Lucid.label_ [Lucid.for_ name, Lucid.class_ "text-sm cursor-pointer"] $ do
        Lucid.span_ [Lucid.class_ "font-bold"] $ do
          Lucid.toHtml $ fromMaybe name (fcLabel cfg)
          when isReq $ Lucid.span_ [Lucid.class_ "text-red-500 ml-1"] "*"

        -- Rich HTML description (if provided)
        forM_ (fcDescriptionHtml cfg) $ \descHtml ->
          Lucid.div_ [Lucid.class_ "text-gray-600 font-normal"] descHtml

        -- Fallback to hint as description (if no descriptionHtml)
        unless hasDescription $
          forM_ (fcHint cfg) $ \h ->
            Lucid.div_ [Lucid.class_ "text-gray-600 font-normal"] (Lucid.toHtml h)

    -- Error message
    when hasVal $
      Lucid.div_
        [ xShow_ ("!fields." <> name <> ".isValid && showErrors"),
          Lucid.class_ (fsErrorMessageClasses styles),
          Lucid.style_ "display: none;"
        ]
        "This field is required"

--------------------------------------------------------------------------------
-- Toggle Field

renderToggleField :: FormStyles -> Field -> Lucid.Html ()
renderToggleField styles field = do
  let name = fName field
      cfg = fConfig field
      isDisabled = fcDisabled cfg
      isChecked = fcChecked cfg
      offLabelText = fromMaybe "Off" (fcOffLabel cfg)
      onLabelText = fromMaybe "On" (fcOnLabel cfg)
      offVal = fromMaybe "off" (fcOffValue cfg)
      onVal = fromMaybe "on" (fcOnValue cfg)
      switchClasses =
        if isDisabled
          then fsToggleSwitchDisabledClasses styles
          else fsToggleSwitchClasses styles
      trackClasses =
        if isDisabled
          then fsToggleTrackDisabledClasses styles
          else fsToggleTrackClasses styles
      toggleId = name <> "-toggle"

  Lucid.div_ [Lucid.class_ "mb-4"] $ do
    -- Optional main label
    forM_ (fcLabel cfg) $ \lbl ->
      Lucid.label_ [Lucid.class_ (fsLabelClasses styles)] (Lucid.toHtml lbl)

    -- Toggle container with Alpine.js state
    Lucid.div_
      [ xData_ [i|{ isOn: #{if isChecked then "true" else "false" :: Text} }|],
        Lucid.class_ (fsToggleContainerClasses styles)
      ]
      $ do
        -- Hidden input that submits the actual value
        Lucid.input_
          [ Lucid.type_ "hidden",
            Lucid.name_ name,
            xBindValue_ [i|isOn ? '#{onVal}' : '#{offVal}'|]
          ]

        -- Off label
        Lucid.span_ [Lucid.class_ (fsToggleOffLabelClasses styles)] $
          Lucid.toHtml offLabelText

        -- Toggle switch
        Lucid.label_ [Lucid.class_ switchClasses] $ do
          Lucid.input_ $
            [ Lucid.type_ "checkbox",
              Lucid.id_ toggleId,
              Lucid.class_ "sr-only peer",
              xModel_ "isOn"
            ]
              <> [Lucid.disabled_ "disabled" | isDisabled]
          Lucid.div_ [Lucid.class_ trackClasses] mempty

        -- On label
        Lucid.span_ [Lucid.class_ (fsToggleOnLabelClasses styles)] $
          Lucid.toHtml onLabelText

    -- Hint
    forM_ (fcHint cfg) $ \h ->
      Lucid.p_ [Lucid.class_ (fsHintClasses styles)] (Lucid.toHtml h)
