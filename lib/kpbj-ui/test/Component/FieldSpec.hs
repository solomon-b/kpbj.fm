-- | Render tests for the field parts: the wrapper, the labels, the hint, and
-- the error message.
module Component.FieldSpec (spec) where

--------------------------------------------------------------------------------

import Component.Field (State (..))
import Component.Field qualified as Field
import Data.Text (Text)
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Component.Field" $ do
  it "renders the field wrapper with the old .fb-field margins" $
    render (Field.field [Lucid.id_ "f"] "x")
      `shouldBe` "<div class=\"mb-4 last:mb-0\" id=\"f\">x</div>"

  it "renders an enabled label with the old .fb-label classes" $
    render (Field.label Enabled [Lucid.for_ "name"] "Name")
      `shouldBe` "<label class=\"block font-bold mb-2 text-[var(--theme-fg)]\" for=\"name\">Name</label>"

  it "renders a disabled label with the muted color" $
    render (Field.label Disabled [Lucid.for_ "name"] "Name")
      `shouldBe` "<label class=\"block font-bold mb-2 text-[var(--theme-fg-muted)]\" for=\"name\">Name</label>"

  it "renders a group label as a paragraph with the label classes" $
    render (Field.groupLabel "Pick one")
      `shouldBe` "<p class=\"block font-bold mb-2 text-[var(--theme-fg)]\">Pick one</p>"

  it "renders a hint with the old .fb-hint classes" $
    render (Field.hint "Help")
      `shouldBe` "<p class=\"text-[0.75rem] text-[var(--theme-fg-muted)] mt-1\">Help</p>"

  it "renders a hidden error message with the old .fb-error classes" $
    render (Field.errorMessage [Lucid.id_ "e"] "Bad")
      `shouldBe` "<span class=\"block mt-1 text-[0.875rem] text-[var(--theme-error)]\" style=\"display: none;\" id=\"e\">Bad</span>"

--------------------------------------------------------------------------------

render :: Lucid.Html () -> Text
render = LText.toStrict . Lucid.renderText
