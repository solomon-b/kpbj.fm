-- | Render tests for the playout source type badge.
--
-- Each expected string is the output of the badge before it moved onto
-- "Component.Badge".
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
  [ ("episode", old "bg-[var(--theme-success)]/10 text-[var(--theme-success)]" "episode"),
    ("ephemeral", old "bg-[var(--theme-info)]/10 text-[var(--theme-info)]" "ephemeral"),
    ("station_id", old "bg-[var(--theme-warning)]/10 text-[var(--theme-warning)]" "station_id"),
    ("psa", old "bg-[var(--theme-warning)]/10 text-[var(--theme-warning)]" "psa"),
    ("underwriting", old "bg-[var(--theme-warning)]/10 text-[var(--theme-warning)]" "underwriting"),
    ("announcement", old "bg-[var(--theme-warning)]/10 text-[var(--theme-warning)]" "announcement"),
    ("show_promo", old "bg-[var(--theme-warning)]/10 text-[var(--theme-warning)]" "show_promo"),
    ("filler", old "bg-[var(--theme-fg-muted)]/10 text-[var(--theme-fg-muted)]" "filler"),
    ("live_dj", old "bg-[var(--theme-bg-inverse)] text-[var(--theme-fg-inverse)]" "live_dj")
  ]
  where
    old :: Text -> Text -> Text
    old tone label = "<span class=\"text-xs px-3 py-2 rounded " <> tone <> "\">" <> label <> "</span>"

render :: Lucid.Html () -> Text
render = LText.toStrict . Lucid.renderText
