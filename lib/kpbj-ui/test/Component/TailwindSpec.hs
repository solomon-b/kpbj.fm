-- | Tests for the shared Tailwind config.
module Component.TailwindSpec (spec) where

--------------------------------------------------------------------------------

import Component.Button (ButtonStyle (..), Kind (..), (&))
import Component.Button qualified as Button
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
  describe "safelist" $
    it "holds every class that a button renders" $
      for_ buttons $ \button ->
        unlisted Tailwind.safelist (Button.render button "x") `shouldBe` []

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

render :: Lucid.Html () -> Text
render = LText.toStrict . Lucid.renderText
