-- | Render tests for the status badges.
module Component.BadgeSpec (spec) where

--------------------------------------------------------------------------------

import Component.Badge (Shape (..), Tone (..))
import Component.Badge qualified as Badge
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Component.Badge" $ do
  describe "Regular" $ do
    it "renders a success badge with the old status pill classes and a tint" $
      classSet (Badge.badge Regular Success "Active")
        `shouldBe` sort (regular <> [tint "success", "text-[var(--theme-success)]"])

    it "renders a warning badge" $
      classSet (Badge.badge Regular Warning "Draft")
        `shouldBe` sort (regular <> [tint "warning", "text-[var(--theme-warning)]"])

    it "renders an error badge" $
      classSet (Badge.badge Regular Error "Revoked")
        `shouldBe` sort (regular <> [tint "error", "text-[var(--theme-error)]"])

    it "renders an info badge" $
      classSet (Badge.badge Regular Info "Host")
        `shouldBe` sort (regular <> [tint "info", "text-[var(--theme-info)]"])

    it "renders a neutral badge with the alternate background and the muted text" $
      classSet (Badge.badge Regular Neutral "Expired")
        `shouldBe` sort (regular <> ["bg-[var(--theme-bg-alt)]", "text-[var(--theme-fg-muted)]"])

    it "renders a plain badge with the alternate background and the main text" $
      classSet (Badge.badge Regular Plain "User")
        `shouldBe` sort (regular <> ["bg-[var(--theme-bg-alt)]", "text-[var(--theme-fg)]"])

  describe "Compact" $ do
    it "renders a warning badge with the old small pill classes" $
      classSet (Badge.badge Compact Warning "DRAFT")
        `shouldBe` sort (compact <> [tint "warning", "text-[var(--theme-warning)]"])

    it "renders a plain badge" $
      classSet (Badge.badge Compact Plain "DRAFT")
        `shouldBe` sort (compact <> ["bg-[var(--theme-bg-alt)]", "text-[var(--theme-fg)]"])

    it "renders an error badge" $
      classSet (Badge.badge Compact Error "ARCHIVED")
        `shouldBe` sort (compact <> [tint "error", "text-[var(--theme-error)]"])

  describe "Tall" $ do
    it "renders an info badge with the old source type classes" $
      classSet (Badge.badge Tall Info "ephemeral")
        `shouldBe` sort (tall <> [tint "info", "text-[var(--theme-info)]"])

    it "renders a muted badge with the muted tint" $
      classSet (Badge.badge Tall Muted "filler")
        `shouldBe` sort (tall <> [tint "fg-muted", "text-[var(--theme-fg-muted)]"])

    it "renders an inverse badge with the inverse colors" $
      classSet (Badge.badge Tall Inverse "other")
        `shouldBe` sort (tall <> ["bg-[var(--theme-bg-inverse)]", "text-[var(--theme-fg-inverse)]"])

  it "renders a span with the label inside" $
    render (Badge.badge Regular Success "Active")
      `shouldSatisfy` (\html -> "<span class=\"" `Text.isPrefixOf` html && "\">Active</span>" `Text.isSuffixOf` html)

--------------------------------------------------------------------------------

regular :: [Text]
regular = ["inline-block", "px-3", "py-1", "text-sm", "font-bold", "rounded"]

compact :: [Text]
compact = ["inline-block", "px-2", "py-1", "rounded", "text-xs", "font-bold"]

tall :: [Text]
tall = ["text-xs", "px-3", "py-2", "rounded"]

-- | The 10% tint of a theme color, such as @tint "success"@.
tint :: Text -> Text
tint color = "bg-[color-mix(in_srgb,var(--theme-" <> color <> ")_10%,transparent)]"

render :: Lucid.Html () -> Text
render = LText.toStrict . Lucid.renderText

-- | The sorted classes of the first @class@ attribute in the HTML.
classSet :: Lucid.Html () -> [Text]
classSet html =
  sort $ Text.words $ Text.takeWhile (/= '"') $ Text.drop 7 $ snd $ Text.breakOn "class=\"" (render html)
