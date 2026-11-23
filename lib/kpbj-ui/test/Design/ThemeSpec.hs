-- | Tests for the theme lookup.
module Design.ThemeSpec (spec) where

--------------------------------------------------------------------------------

import Design.Theme (Theme (..), getTheme)
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Design.Theme.getTheme" $ do
  it "finds each named theme" $ do
    themeName (getTheme "Default") `shouldBe` "default"
    themeName (getTheme "Solarized") `shouldBe` "solarized"
    themeName (getTheme "Gruvbox") `shouldBe` "gruvbox"
    themeName (getTheme "Dracula") `shouldBe` "dracula"
    themeName (getTheme "Nord") `shouldBe` "nord"

  it "falls back to the default theme for an unknown name" $ do
    themeName (getTheme "") `shouldBe` "default"
    themeName (getTheme "solarized") `shouldBe` "default"
