-- | Render tests for the button component.
module Component.ButtonSpec (spec) where

--------------------------------------------------------------------------------

import Component.Button (ButtonStyle (..), Corners (..), Kind (..), Size (..), Tone (..), (&))
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

    it "renders an accent button with the accent colors and a light fade" $ do
      let classes = classList (render (Button.defaults & Button.accent) "Go")
      mapM_ (\c -> classes `shouldContain` [c]) ["bg-[var(--theme-accent)]", "text-[var(--theme-accent-fg)]", "hover:opacity-90"]
      classes `shouldNotContain` ["hover:opacity-80"]

    it "renders a muted button with the alternate background and a hover background" $ do
      let classes = classList (render (Button.defaults & Button.muted) "Back")
      mapM_ (\c -> classes `shouldContain` [c]) [Tokens.bgAlt, Tokens.hoverBg]
      classes `shouldNotContain` ["hover:opacity-80"]

    it "renders a bare button with the text color of its tone, a hover background, and no fill" $ do
      let classes = classList (render (Button.defaults & Button.bare Info) "LOGIN")
      mapM_ (\c -> classes `shouldContain` [c]) [Tokens.infoText, Tokens.hoverBg]
      filter (Text.isPrefixOf "bg-") classes `shouldBe` []

  describe "standard output" $ do
    it "renders a primary submit with these exact classes" $
      render (Button.defaults & Button.primary & Button.submit) "Save"
        `shouldBe` "<button type=\"submit\" class=\"font-bold whitespace-nowrap cursor-pointer no-underline transition-all duration-150 ease-[ease-in-out] disabled:opacity-50 disabled:cursor-not-allowed px-3 py-2 text-sm rounded bg-[var(--theme-bg-inverse)] text-[var(--theme-fg-inverse)] hover:opacity-80\">Save</button>"

    it "renders a small full width secondary link with these exact classes" $
      render (Button.defaults & Button.small & Button.fullWidth & Button.pageLink "/x") "View"
        `shouldBe` "<a href=\"/x\" class=\"font-bold whitespace-nowrap cursor-pointer no-underline transition-all duration-150 ease-[ease-in-out] disabled:opacity-50 disabled:cursor-not-allowed px-2 py-1 text-sm rounded w-full bg-[var(--theme-button)] text-[var(--theme-fg)] hover:opacity-80\">View</a>"

  -- The public forms had these looks in @forms.css@. 'oldFormSubmit' and
  -- 'oldFormCancel' give each old rule as Tailwind classes, written by hand.
  -- The modifiers must give the same classes, plus only classes that do not
  -- change these buttons. The test holds the looks in place. It does not
  -- prove that the hand-written lists equal the old CSS.
  describe "public form looks" $ do
    it "gives the old form submit classes with accent, large, and square" $
      classList (render (Button.defaults & Button.accent & Button.large & Button.square & Button.submit) "LOGIN")
        `shouldMatchList` (oldFormSubmit <> ["whitespace-nowrap", "no-underline"])

    it "gives the old form cancel classes with muted, large, and square" $
      classList (render (Button.defaults & Button.muted & Button.large & Button.square & Button.pageLink "/") "CANCEL")
        `shouldMatchList` (oldFormCancel <> ["whitespace-nowrap", "cursor-pointer", "disabled:opacity-50", "disabled:cursor-not-allowed"])

  describe "sizes, corners, and widths" $ do
    it "renders the default size" $ do
      let html = render Button.defaults "x"
      html `shouldSatisfy` Text.isInfixOf "px-3"
      html `shouldSatisfy` Text.isInfixOf "py-2"

    it "renders the small size" $ do
      let html = render (Button.defaults & Button.small) "x"
      html `shouldSatisfy` Text.isInfixOf "px-2"
      html `shouldSatisfy` Text.isInfixOf "py-1"
      html `shouldNotSatisfy` Text.isInfixOf "py-2"

    it "renders the large size with no text size" $ do
      let classes = classList (render (Button.defaults & Button.large) "x")
      mapM_ (\c -> classes `shouldContain` [c]) ["px-6", "py-3"]
      classes `shouldNotContain` [Tokens.textSm]

    it "renders square corners" $
      classList (render (Button.defaults & Button.square) "x") `shouldNotContain` ["rounded"]

    it "renders the full width" $ do
      let html = render (Button.defaults & Button.fullWidth) "x"
      html `shouldSatisfy` Text.isInfixOf "w-full"

  describe "modifiers" $ do
    it "lets the later style win" $
      (Button.defaults & Button.primary & Button.danger) ^. Button.style `shouldBe` Danger

    it "keeps the size when the style changes" $
      (Button.defaults & Button.small & Button.accent) ^. Button.size `shouldBe` Small

    it "keeps the style when the size changes" $
      (Button.defaults & Button.accent & Button.large) ^. Button.style `shouldBe` Accent

    it "lets a lens set a field after a modifier" $
      (Button.defaults & Button.small & Button.size .~ Medium) ^. Button.size `shouldBe` Medium

    it "sets the corners" $
      (Button.defaults & Button.square) ^. Button.corners `shouldBe` Square

    it "lets the later kind win" $
      (Button.defaults & Button.submit & Button.pageLink "/x") ^. Button.kind `shouldBe` PageLink "/x"

    it "adds attribute lists together" $ do
      let html = render (Button.defaults & Button.with [Lucid.id_ "a"] & Button.with [Lucid.title_ "b"]) "x"
      html `shouldSatisfy` Text.isInfixOf "id=\"a\""
      html `shouldSatisfy` Text.isInfixOf "title=\"b\""

    it "adds the classes from an attribute to the button classes" $ do
      let classes = classList (render (Button.defaults & Button.with [Lucid.class_ "absolute"]) "x")
      classes `shouldContain` ["absolute"]
      classes `shouldContain` [Tokens.bgButton]

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

    it "renders a label for an input as an inline block" $ do
      let html = render (Button.defaults & Button.primary & Button.labelFor "photo-input") "SELECT FILE"
      html `shouldSatisfy` Text.isPrefixOf "<label for=\"photo-input\""
      classList html `shouldContain` ["inline-block"]
      classList html `shouldContain` [Tokens.bgInverse]

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

-- | The classes of the old @.fb-submit@ rule of @forms.css@.
oldFormSubmit :: [Text]
oldFormSubmit =
  ["px-6", "py-3", "bg-[var(--theme-accent)]", "text-[var(--theme-accent-fg)]", "font-bold", "cursor-pointer", "transition-all", "duration-150", "ease-[ease-in-out]", "hover:opacity-90", "disabled:opacity-50", "disabled:cursor-not-allowed"]

-- | The classes of the old @.fb-cancel@ rule of @forms.css@.
oldFormCancel :: [Text]
oldFormCancel =
  ["px-6", "py-3", "bg-[var(--theme-bg-alt)]", "text-[var(--theme-fg)]", "font-bold", "no-underline", "transition-all", "duration-150", "ease-[ease-in-out]", "hover:bg-[var(--theme-hover)]"]

render :: Button.Spec -> Text -> Text
render button label = LText.toStrict . Lucid.renderText $ Button.render button (Lucid.toHtml label)

-- | The value of the first attribute with the given name.
attrValue :: Text -> Text -> Text
attrValue name html =
  let (_, rest) = Text.breakOn (" " <> name <> "=\"") html
   in Text.takeWhile (/= '"') (Text.drop (Text.length name + 3) rest)

-- | The classes of the first @class@ attribute.
classList :: Text -> [Text]
classList = Text.words . attrValue "class"
