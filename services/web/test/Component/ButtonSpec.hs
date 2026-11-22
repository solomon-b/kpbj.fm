-- | Render tests for the dashboard button component.
module Component.ButtonSpec (spec) where

--------------------------------------------------------------------------------

import Component.Button (ButtonStyle (..), Kind (..), Size (..), Tone (..), (&))
import Component.Button qualified as Button
import Control.Lens ((.~), (^.))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Design.Tokens qualified as Tokens
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Component.Button" $ do
  describe "styles" $ do
    it "renders a primary button with the inverse colors" $ do
      let html = render (Button.defaults & Button.primary) "Go"
      html `shouldSatisfy` Text.isInfixOf "rounded"
      html `shouldSatisfy` Text.isInfixOf Tokens.bgInverse

    it "renders a danger button with the error text" $ do
      let html = render (Button.defaults & Button.danger) "Delete"
      html `shouldSatisfy` Text.isInfixOf Tokens.errorText

    it "renders a tinted button with a tint and the text color of its tone" $ do
      let html = render (Button.defaults & Button.tinted Warning) "Restart"
      html `shouldSatisfy` Text.isInfixOf "bg-[color-mix(in_srgb,var(--theme-warning)_15%,transparent)]"
      html `shouldSatisfy` Text.isInfixOf Tokens.warningText

    it "renders the secondary colors by default" $ do
      let html = render Button.defaults "Save"
      html `shouldSatisfy` Text.isInfixOf Tokens.bgButton
      html `shouldSatisfy` Text.isInfixOf Tokens.fgPrimary

  describe "sizes and widths" $ do
    it "renders the default size" $ do
      let html = render Button.defaults "x"
      html `shouldSatisfy` Text.isInfixOf "px-3"
      html `shouldSatisfy` Text.isInfixOf "py-2"

    it "renders the small size" $ do
      let html = render (Button.defaults & Button.small) "x"
      html `shouldSatisfy` Text.isInfixOf "px-2"
      html `shouldSatisfy` Text.isInfixOf "py-1"
      html `shouldNotSatisfy` Text.isInfixOf "py-2"

    it "renders the full width" $ do
      let html = render (Button.defaults & Button.fullWidth) "x"
      html `shouldSatisfy` Text.isInfixOf "w-full"

  describe "modifiers" $ do
    it "lets the later style win" $
      (Button.defaults & Button.primary & Button.danger) ^. Button.style `shouldBe` Danger

    it "lets a lens set a field after a modifier" $
      (Button.defaults & Button.small & Button.size .~ Medium) ^. Button.size `shouldBe` Medium

    it "lets the later kind win" $
      (Button.defaults & Button.submit & Button.pageLink "/x") ^. Button.kind `shouldBe` PageLink "/x"

    it "adds attribute lists together" $ do
      let html = render (Button.defaults & Button.with [Lucid.id_ "a"] & Button.with [Lucid.title_ "b"]) "x"
      html `shouldSatisfy` Text.isInfixOf "id=\"a\""
      html `shouldSatisfy` Text.isInfixOf "title=\"b\""

  describe "kinds" $ do
    it "renders a plain button by default" $ do
      let html = render Button.defaults "x"
      html `shouldSatisfy` Text.isInfixOf "<button"
      html `shouldSatisfy` Text.isInfixOf "type=\"button\""

    it "renders a submit button" $ do
      let html = render (Button.defaults & Button.submit) "Save"
      html `shouldSatisfy` Text.isInfixOf "type=\"submit\""

    it "renders a dashboard link that swaps the page into the frame" $ do
      let html = render (Button.defaults & Button.dashboardLink "/x") "Edit"
      html `shouldSatisfy` Text.isInfixOf "<a"
      html `shouldSatisfy` Text.isInfixOf "href=\"/x\""
      html `shouldSatisfy` Text.isInfixOf "hx-get=\"/x\""
      html `shouldSatisfy` Text.isInfixOf "hx-target=\"#main-content\""
      html `shouldSatisfy` Text.isInfixOf "hx-push-url=\"true\""

    it "renders a page link as a plain link" $ do
      let html = render (Button.defaults & Button.pageLink "/x") "View"
      html `shouldSatisfy` Text.isInfixOf "href=\"/x\""
      html `shouldNotSatisfy` Text.isInfixOf "hx-get"

    it "keeps extra attributes on a page link" $ do
      let html = render (Button.defaults & Button.pageLink "#now" & Button.with [Lucid.onclick_ "f()"]) "NOW"
      html `shouldSatisfy` Text.isInfixOf "href=\"#now\""
      html `shouldSatisfy` Text.isInfixOf "onclick=\"f()\""

  describe "toggle" $ do
    let html = render (Button.defaults & Button.small & Button.primary & Button.toggle "a === 1" Secondary) "One"

    it "renders a plain button" $
      html `shouldSatisfy` Text.isInfixOf "type=\"button\""

    it "switches between both color sets in x-bind:class" $ do
      let bound = attrValue "x-bind:class" html
      bound `shouldSatisfy` Text.isInfixOf Tokens.bgInverse
      bound `shouldSatisfy` Text.isInfixOf Tokens.bgButton
      bound `shouldSatisfy` Text.isInfixOf "a === 1"

    it "has the shape classes of its spec and no color class in class" $ do
      let static = attrValue "class" html
      static `shouldSatisfy` Text.isInfixOf "rounded"
      static `shouldSatisfy` Text.isInfixOf "py-1"
      static `shouldNotSatisfy` Text.isInfixOf Tokens.bgInverse
      static `shouldNotSatisfy` Text.isInfixOf Tokens.bgButton

--------------------------------------------------------------------------------

render :: Button.Spec -> Text -> Text
render button label = LText.toStrict . Lucid.renderText $ Button.render button (Lucid.toHtml label)

-- | The value of the first attribute with the given name.
attrValue :: Text -> Text -> Text
attrValue name html =
  let (_, rest) = Text.breakOn (" " <> name <> "=\"") html
   in Text.takeWhile (/= '"') (Text.drop (Text.length name + 3) rest)
