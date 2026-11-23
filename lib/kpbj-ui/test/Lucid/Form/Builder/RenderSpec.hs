-- | Render tests for the footer buttons of the form builder.
module Lucid.Form.Builder.RenderSpec (spec) where

--------------------------------------------------------------------------------

import Component.Button ((&))
import Component.Button qualified as Button
import Component.Tailwind qualified as Tailwind
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Lucid.Form.Builder
  ( FooterLook (..),
    FormConfig (..),
    cancelButton,
    defaultFormConfig,
    imagesField,
    label,
    renderForm,
    submitButton,
  )
import Test.Classes (unlisted)
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Lucid.Form.Builder.Render footer" $ do
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
  describe "the images field" $
    it "renders no class that is outside the safelist and forms.css" $ do
      let form = renderForm defaultFormConfig {fcAction = "/save"} (imagesField "photos" (label "Photos"))
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

--------------------------------------------------------------------------------

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
