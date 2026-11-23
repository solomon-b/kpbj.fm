-- | Render tests for the cards.
module Component.CardSpec (spec) where

--------------------------------------------------------------------------------

import Component.Card (Variant (..))
import Component.Card qualified as Card
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Component.Card" $ do
  it "renders an empty state card with the old classes" $
    classSet (Card.card EmptyState [] "x")
      `shouldBe` sort ["bg-[var(--theme-bg-alt)]", "border-2", "border-[var(--theme-border-muted)]", "p-12", "text-center"]

  it "renders a header card with the old classes" $
    classSet (Card.card Header [] "x")
      `shouldBe` sort ["bg-[var(--theme-bg)]", "text-[var(--theme-fg)]", "p-6", "mb-8", "w-full"]

  it "renders an auth card with the old classes" $
    classSet (Card.card Auth [] "x")
      `shouldBe` sort ["bg-[var(--theme-bg)]", "p-8"]

  it "renders a detail card with the old classes" $
    classSet (Card.card Detail [] "x")
      `shouldBe` sort ["bg-[var(--theme-bg)]", "rounded", "p-6"]

  it "renders an inset card with the old classes" $
    classSet (Card.card Inset [] "x")
      `shouldBe` sort ["bg-[var(--theme-bg-alt)]", "border-2", "border-[var(--theme-border-muted)]", "p-4"]

  it "renders a table panel card with the old classes" $
    classSet (Card.card TablePanel [] "x")
      `shouldBe` sort ["bg-[var(--theme-bg)]", "rounded", "overflow-hidden"]

  it "renders a table panel section with a bottom margin" $
    render (Card.section TablePanel ["mb-8"] "x")
      `shouldBe` "<section class=\"bg-[var(--theme-bg)] rounded overflow-hidden mb-8\">x</section>"

  it "adds the extra classes of the caller" $
    classSet (Card.card Auth ["text-center"] "x")
      `shouldBe` sort ["bg-[var(--theme-bg)]", "p-8", "text-center"]

  it "renders a card as a div with the content inside" $
    render (Card.card Detail ["mb-6"] "x")
      `shouldBe` "<div class=\"bg-[var(--theme-bg)] rounded p-6 mb-6\">x</div>"

  it "renders a card as a section" $
    render (Card.section Detail ["mb-8"] "x")
      `shouldBe` "<section class=\"bg-[var(--theme-bg)] rounded p-6 mb-8\">x</section>"

--------------------------------------------------------------------------------

render :: Lucid.Html () -> Text
render = LText.toStrict . Lucid.renderText

-- | The sorted classes of the first @class@ attribute in the HTML.
classSet :: Lucid.Html () -> [Text]
classSet html =
  sort $ Text.words $ Text.takeWhile (/= '"') $ Text.drop 7 $ snd $ Text.breakOn "class=\"" (render html)
