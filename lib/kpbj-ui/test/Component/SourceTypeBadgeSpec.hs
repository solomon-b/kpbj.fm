-- | Render tests for the playout source type badge.
--
-- Each expected string is the output of the badge before it moved onto
-- "Component.Badge". The only change is the status tint, which replaced the
-- opacity modifier that Tailwind did not render.
module Component.SourceTypeBadgeSpec (spec) where

--------------------------------------------------------------------------------

import Component.SourceTypeBadge (sourceTypeBadge)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Component.SourceTypeBadge" $
  for_ cases $ \(sourceType, expected) ->
    it ("renders " <> show sourceType <> " as before") $
      render (sourceTypeBadge sourceType) `shouldBe` expected

--------------------------------------------------------------------------------

cases :: [(Text, Text)]
cases =
  [ ("episode", old (tint "success" <> " text-[var(--theme-success)]") "episode"),
    ("ephemeral", old (tint "info" <> " text-[var(--theme-info)]") "ephemeral"),
    ("station_id", old (tint "warning" <> " text-[var(--theme-warning)]") "station_id"),
    ("psa", old (tint "warning" <> " text-[var(--theme-warning)]") "psa"),
    ("underwriting", old (tint "warning" <> " text-[var(--theme-warning)]") "underwriting"),
    ("announcement", old (tint "warning" <> " text-[var(--theme-warning)]") "announcement"),
    ("show_promo", old (tint "warning" <> " text-[var(--theme-warning)]") "show_promo"),
    ("filler", old (tint "fg-muted" <> " text-[var(--theme-fg-muted)]") "filler"),
    ("live_dj", old "bg-[var(--theme-bg-inverse)] text-[var(--theme-fg-inverse)]" "live_dj")
  ]
  where
    old :: Text -> Text -> Text
    old tone label = "<span class=\"text-xs px-3 py-2 rounded " <> tone <> "\">" <> label <> "</span>"

    -- The 10% tint of a theme color.
    tint :: Text -> Text
    tint color = "bg-[color-mix(in_srgb,var(--theme-" <> color <> ")_10%,transparent)]"

render :: Lucid.Html () -> Text
render = LText.toStrict . Lucid.renderText
