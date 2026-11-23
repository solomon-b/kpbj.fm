-- | Tests for the shared Tailwind config.
module Component.TailwindSpec (spec) where

--------------------------------------------------------------------------------

import Component.Badge qualified as Badge
import Component.Button (ButtonStyle (..), Kind (..), (&))
import Component.Button qualified as Button
import Component.Card qualified as Card
import Component.Choice qualified as Choice
import Component.Field qualified as Field
import Component.Input qualified as Input
import Component.Modal qualified as Modal
import Component.Tailwind qualified as Tailwind
import Control.Lens ((.~))
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Test.Classes (unlisted)
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Component.Tailwind" $ do
  describe "safelist" $ do
    it "holds every class that a button renders" $
      for_ buttons $ \button ->
        unlisted Tailwind.safelist (Button.render button "x") `shouldBe` []

    it "holds every class that a form control renders" $
      unlisted Tailwind.safelist formControls `shouldBe` []

    it "holds every class that a badge, a card, or a modal renders" $
      unlisted Tailwind.safelist panels `shouldBe` []

  it "keeps max-h-64 in the safelist" $
    Tailwind.safelist `shouldContain` ["max-h-64"]

  it "renders the safelist as JSON in the config script" $ do
    let html = render Tailwind.configScript
    html `shouldSatisfy` Text.isInfixOf "tailwind.config = { darkMode: 'class', safelist: [\""
    html `shouldSatisfy` Text.isInfixOf "\"max-h-64\","
    html `shouldSatisfy` Text.isInfixOf "\"], theme: {"

  it "sets the monospace font stack" $
    render Tailwind.configScript
      `shouldSatisfy` Text.isInfixOf "sans: ['ui-monospace', 'SFMono-Regular'"

--------------------------------------------------------------------------------

-- | A button for each style, size, corner, width, and kind.
buttons :: [Button.Spec]
buttons =
  [ Button.defaults
      & (Button.style .~ style)
      & (Button.size .~ size)
      & (Button.corners .~ corners)
      & (Button.width .~ width)
      & (Button.kind .~ kind)
  | style <- styles,
    size <- [minBound .. maxBound],
    corners <- [minBound .. maxBound],
    width <- [minBound .. maxBound],
    kind <- kinds
  ]
  where
    styles = [Primary, Secondary, Danger, Accent, Muted] <> map Tinted [minBound .. maxBound]
    kinds =
      [Action, Submit, DashboardLink "/x", PageLink "/x", LabelFor "x"]
        <> map (Toggle "open") styles

-- | Each part of "Component.Field", "Component.Input", and
-- "Component.Choice", in each state.
formControls :: Lucid.Html ()
formControls = do
  Field.field [] "x"
  for_ [minBound .. maxBound] $ \state -> Field.label state [] "x"
  Field.groupLabel "x"
  Field.hint "x"
  Field.errorMessage [] "x"
  Input.input [Input.errorWhen "bad"]
  Input.textarea [Input.errorWhen "bad"] "x"
  Input.select [Input.errorWhen "bad"] "x"
  for_ [minBound .. maxBound] $ \requirement ->
    Choice.checkbox "a" [] "x" requirement (Just "x")
  Choice.radioGroup [Choice.radioErrorWhen "bad"] $
    Choice.radioOption "b" [] "x" (Just "x")

-- | Each badge, card, and modal, in each variant.
panels :: Lucid.Html ()
panels = do
  sequence_ [Badge.badge shape tone "x" | shape <- [minBound .. maxBound], tone <- [minBound .. maxBound]]
  for_ [minBound .. maxBound] $ \variant -> do
    Card.card variant [] "x"
    Card.section variant [] "x"
  for_ [minBound .. maxBound] $ \look -> Modal.modal look "open" "open = false" [] "x"

render :: Lucid.Html () -> Text
render = LText.toStrict . Lucid.renderText
