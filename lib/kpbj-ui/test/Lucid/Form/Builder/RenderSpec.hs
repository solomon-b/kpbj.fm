-- | Render tests for the footer buttons and the basic inputs of the form
-- builder.
module Lucid.Form.Builder.RenderSpec (spec) where

--------------------------------------------------------------------------------

import Component.Button ((&))
import Component.Button qualified as Button
import Component.Input qualified as Input
import Component.Tailwind qualified as Tailwind
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Lucid.Form.Builder
  ( FooterLook (..),
    FormBuilder,
    FormConfig (..),
    addOption,
    addOptionWithDesc,
    audioField,
    cancelButton,
    checkboxField,
    dateField,
    dateTimeField,
    defaultFormConfig,
    disabled,
    fileField,
    hint,
    imageField,
    imagesField,
    label,
    numberField,
    passwordField,
    radioField,
    renderForm,
    required,
    selectField,
    stagedAudioField,
    stagedImageField,
    submitButton,
    textField,
    textareaField,
    toggleField,
    urlField,
  )
import Test.Classes (unlisted)
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  footerSpec
  inputSpec

footerSpec :: Spec
footerSpec = describe "Lucid.Form.Builder.Render footer" $ do
  describe "the public footer" $ do
    let html = renderFooter defaultFormConfig

    it "is the default" $
      fcFooterLook defaultFormConfig `shouldBe` PublicFooter

    it "renders the submit as an accent, large, square button" $
      html `shouldSatisfy` Text.isInfixOf (classAttr publicSubmit)

    it "renders the cancel as a muted, large, square button" $
      html `shouldSatisfy` Text.isInfixOf (classAttr publicCancel)

    it "renders no fb class on a button" $
      for_ ["fb-submit", "fb-cancel", "fb-submit--loading"] $ \c ->
        html `shouldNotSatisfy` Text.isInfixOf c

  describe "the dashboard footer" $ do
    let html = renderFooter defaultFormConfig {fcFooterLook = DashboardFooter}

    it "renders the submit as a primary button" $
      html `shouldSatisfy` Text.isInfixOf (classAttr (Button.defaults & Button.primary))

    it "renders the cancel as a secondary button" $
      html `shouldSatisfy` Text.isInfixOf (classAttr Button.defaults)

    it "renders no public look" $
      for_ [publicSubmit, publicCancel] $ \button ->
        html `shouldNotSatisfy` Text.isInfixOf (classAttr button)

  describe "the submit" $
    for_ [PublicFooter, DashboardFooter] $ \look -> do
      let html = renderFooter defaultFormConfig {fcFooterLook = look}

      it ("is a submit button in the " <> show look) $
        html `shouldSatisfy` Text.isInfixOf "<button type=\"submit\""

      it ("is disabled during an upload in the " <> show look) $
        html `shouldSatisfy` Text.isInfixOf "x-bind:disabled=\"isUploading || stagedUploading\""

  -- A form can first appear in an HTMX swap, and each @fb-@ class has a rule
  -- in @forms.css@. Every other class needs the safelist.
  describe "a form with every kind of field" $
    it "renders no class that is outside the safelist and forms.css" $ do
      let form = renderForm defaultFormConfig {fcAction = "/save"} everyField
      filter (not . Text.isPrefixOf "fb-") (unlisted Tailwind.safelist form) `shouldBe` []

  describe "the cancel link" $ do
    it "swaps into the dashboard frame for the #main-content target" $ do
      let html = renderFooter defaultFormConfig {fcHtmxTarget = Just "#main-content"}
      html `shouldSatisfy` Text.isInfixOf "href=\"/back\""
      html `shouldSatisfy` Text.isInfixOf "hx-get=\"/back\""
      html `shouldSatisfy` Text.isInfixOf "hx-target=\"#main-content\""
      html `shouldSatisfy` Text.isInfixOf "hx-push-url=\"true\""

    it "swaps into another target" $ do
      let html = renderFooter defaultFormConfig {fcHtmxTarget = Just "body"}
      html `shouldSatisfy` Text.isInfixOf "href=\"/back\""
      html `shouldSatisfy` Text.isInfixOf "hx-get=\"/back\""
      html `shouldSatisfy` Text.isInfixOf "hx-target=\"body\""
      html `shouldSatisfy` Text.isInfixOf "hx-push-url=\"true\""

    it "is a plain link with no target" $ do
      let html = renderFooter defaultFormConfig
      html `shouldSatisfy` Text.isInfixOf "href=\"/back\""
      html `shouldNotSatisfy` Text.isInfixOf "hx-get"
      html `shouldNotSatisfy` Text.isInfixOf "hx-push-url"

inputSpec :: Spec
inputSpec = describe "Lucid.Form.Builder.Render inputs" $ do
  describe "a text field" $ do
    let html = renderFields $ textField "title" (label "Title" >> required >> hint "Short")

    it "renders the field, label, input, and hint components" $ do
      html `shouldSatisfy` Text.isInfixOf "<div class=\"mb-4 last:mb-0\">"
      html `shouldSatisfy` Text.isInfixOf "<label class=\"block font-bold mb-2 text-[var(--theme-fg)]\" for=\"title\">Title *</label>"
      html `shouldSatisfy` Text.isInfixOf "<input class=\"w-full p-3 border"
      html `shouldSatisfy` Text.isInfixOf "<p class=\"text-[0.75rem] text-[var(--theme-fg-muted)] mt-1\">Short</p>"

    it "keeps its Alpine attributes" $ do
      html `shouldSatisfy` Text.isInfixOf "x-model=\"fields.title.value\""
      html `shouldSatisfy` Text.isInfixOf "x-on:input=\"showErrors &amp;&amp; validateTitle()\""
      html `shouldSatisfy` Text.isInfixOf "x-on:blur=\"showErrors &amp;&amp; validateTitle()\""
      html `shouldSatisfy` Text.isInfixOf "x-show=\"!fields.title.isValid &amp;&amp; showErrors\""

    it "toggles the error class of the input component" $
      html `shouldSatisfy` Text.isInfixOf ("&#39;" <> Input.errorClass <> "&#39;: showErrors &amp;&amp; !fields.title.isValid")

    it "renders the error message component" $
      html `shouldSatisfy` Text.isInfixOf "class=\"block mt-1 text-[0.875rem] text-[var(--theme-error)]\" style=\"display: none;\""

  describe "a disabled text field" $ do
    let html = renderFields $ textField "locked" (label "Locked" >> disabled)

    it "renders the muted label" $
      html `shouldSatisfy` Text.isInfixOf "<label class=\"block font-bold mb-2 text-[var(--theme-fg-muted)]\" for=\"locked\">Locked</label>"

    it "disables the input" $
      html `shouldSatisfy` Text.isInfixOf "disabled=\"\""

  describe "a select field" $ do
    let html = renderFields $ selectField "kind" (label "Kind" >> required >> addOption "a" "A")

    it "renders the select component with its Alpine attributes" $ do
      html `shouldSatisfy` Text.isInfixOf "<select class=\"w-full p-3 border"
      html `shouldSatisfy` Text.isInfixOf "cursor-pointer disabled:cursor-default\""
      html `shouldSatisfy` Text.isInfixOf "x-model=\"fields.kind.value\""
      html `shouldSatisfy` Text.isInfixOf "x-on:change=\"showErrors &amp;&amp; validateKind()\""
      html `shouldSatisfy` Text.isInfixOf ("&#39;" <> Input.errorClass <> "&#39;: showErrors &amp;&amp; !fields.kind.isValid")

  describe "a textarea field" $ do
    let html = renderFields $ textareaField "body" 4 (label "Body" >> required)

    it "renders the textarea component with its Alpine attributes" $ do
      html `shouldSatisfy` Text.isInfixOf "<textarea class=\"w-full p-3 border"
      html `shouldSatisfy` Text.isInfixOf "leading-relaxed resize-y\""
      html `shouldSatisfy` Text.isInfixOf "rows=\"4\""
      html `shouldSatisfy` Text.isInfixOf "x-model=\"fields.body.value\""
      html `shouldSatisfy` Text.isInfixOf ("&#39;" <> Input.errorClass <> "&#39;: showErrors &amp;&amp; !fields.body.isValid")

  describe "a checkbox field" $ do
    let html = renderFields $ checkboxField "agree" (label "Agree" >> required >> hint "Terms")

    it "renders the checkbox component with the required mark and the description" $ do
      html `shouldSatisfy` Text.isInfixOf "<input type=\"checkbox\" id=\"agree\" class=\"mt-1 cursor-pointer disabled:cursor-default\""
      html `shouldSatisfy` Text.isInfixOf "<span class=\"text-[var(--theme-error)] ml-1\">*</span>"
      html `shouldSatisfy` Text.isInfixOf "<div class=\"text-[0.875rem] text-[var(--theme-fg-muted)] mt-1\">Terms</div>"

    it "keeps its Alpine attributes" $ do
      html `shouldSatisfy` Text.isInfixOf "x-model=\"fields.agree.value\""
      html `shouldSatisfy` Text.isInfixOf "x-on:change=\"showErrors &amp;&amp; validateAgree()\""

  describe "a radio field" $ do
    let html = renderFields $ radioField "choice" (label "Choice" >> required >> addOptionWithDesc "x" "X" "The x")

    it "renders the group label, the group, and the option components" $ do
      html `shouldSatisfy` Text.isInfixOf "<p class=\"block font-bold mb-2 text-[var(--theme-fg)]\">Choice *</p>"
      html `shouldSatisfy` Text.isInfixOf "<div class=\"flex flex-col gap-2\""
      html `shouldSatisfy` Text.isInfixOf "<input type=\"radio\" id=\"choice-x\" class=\"mt-1 cursor-pointer disabled:cursor-default\""
      html `shouldSatisfy` Text.isInfixOf "<span class=\"text-[0.875rem] text-[var(--theme-fg-muted)] ml-6\">The x</span>"

    it "keeps its Alpine attributes" $ do
      html `shouldSatisfy` Text.isInfixOf "&#39;p-2 -m-2 border border-[var(--theme-error)]&#39;: showErrors &amp;&amp; !fields.choice.isValid"
      html `shouldSatisfy` Text.isInfixOf "x-model=\"fields.choice.value\""
      html `shouldSatisfy` Text.isInfixOf "x-on:change=\"showErrors &amp;&amp; validateChoice()\""

  it "emits no fb class for a basic input" $ do
    let html =
          renderFields $ do
            textField "title" (label "Title" >> required >> hint "Short")
            textField "locked" (label "Locked" >> disabled)
            selectField "kind" (label "Kind" >> required >> addOption "a" "A")
            textareaField "body" 4 (label "Body" >> required)
            checkboxField "agree" (label "Agree" >> required >> hint "Terms")
            radioField "choice" (label "Choice" >> required >> addOptionWithDesc "x" "X" "The x")
    for_ ["fb-field", "fb-label", "fb-input", "fb-textarea", "fb-select", "fb-hint", "fb-error", "fb-radio", "fb-checkbox"] $ \c ->
      html `shouldNotSatisfy` Text.isInfixOf c

--------------------------------------------------------------------------------

-- | One field of each kind, in the enabled and the disabled state.
everyField :: FormBuilder
everyField = do
  for_ [pure (), disabled] $ \state -> do
    let field = label "Field" >> required >> hint "Hint" >> state
    textField "text" field
    urlField "url" field
    passwordField "password" field
    textareaField "textarea" 4 field
    selectField "select" (field >> addOption "a" "A")
    radioField "radio" (field >> addOptionWithDesc "x" "X" "The x")
    fileField "file" (Just "image/*") field
    imageField "image" field
    imagesField "images" field
    audioField "audio" field
    stagedAudioField "staged-audio" "/upload" "episode" field
    stagedImageField "staged-image" "/upload" "cover" field
    dateTimeField "datetime" field
    dateField "date" field
    numberField "number" (Just 0) (Just 10) (Just 1) field
    checkboxField "checkbox" field
    toggleField "toggle" field

-- | Render a form that has only the given fields.
renderFields :: FormBuilder -> Text
renderFields = LText.toStrict . Lucid.renderText . renderForm defaultFormConfig {fcAction = "/save"}

-- | Render a form that has only a submit and a cancel button.
renderFooter :: FormConfig -> Text
renderFooter config =
  LText.toStrict . Lucid.renderText $
    renderForm config {fcAction = "/save"} $ do
      submitButton "SAVE"
      cancelButton "/back" "CANCEL"

-- | The class attribute of a button with the given spec.
classAttr :: Button.Spec -> Text
classAttr button =
  let html = LText.toStrict . Lucid.renderText $ Button.render button "x"
      (_, rest) = Text.breakOn "class=\"" html
   in "class=\"" <> Text.takeWhile (/= '"') (Text.drop 7 rest) <> "\""

-- | The submit of a public form.
publicSubmit :: Button.Spec
publicSubmit = Button.defaults & Button.accent & Button.large & Button.square

-- | The cancel link of a public form.
publicCancel :: Button.Spec
publicCancel = Button.defaults & Button.muted & Button.large & Button.square
