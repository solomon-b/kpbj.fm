-- | Tests for the shared Tailwind config.
module Component.TailwindSpec (spec) where

--------------------------------------------------------------------------------

import Component.Button qualified as Button
import Component.Tailwind qualified as Tailwind
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Component.Tailwind" $ do
  it "puts every button class in the safelist" $
    for_ Button.safelist $
      \c -> Tailwind.safelist `shouldContain` [c]

  it "keeps max-h-64 in the safelist" $
    Tailwind.safelist `shouldContain` ["max-h-64"]

  it "renders the safelist as JSON in the config script" $ do
    let html = render Tailwind.configScript
    html `shouldSatisfy` Text.isInfixOf "tailwind.config = { darkMode: 'class', safelist: [\""
    html `shouldSatisfy` Text.isInfixOf "\"max-h-64\"]"

  it "sets the monospace font stack" $
    render Tailwind.configScript
      `shouldSatisfy` Text.isInfixOf "sans: ['ui-monospace', 'SFMono-Regular'"

render :: Lucid.Html () -> Text
render = LText.toStrict . Lucid.renderText
