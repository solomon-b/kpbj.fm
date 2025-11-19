module Domain.Types.SlugSpec (spec) where

--------------------------------------------------------------------------------

import Data.Text qualified as Text
import Domain.Types.Slug (Slug (..), mkSlug)
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Domain.Types.Slug" $ do
  describe "mkSlug" $ do
    it "converts spaces to hyphens" $ do
      mkSlug "Hello World" `shouldBe` Slug "hello-world"
      mkSlug "Hello  World" `shouldBe` Slug "hello-world"
      mkSlug "My Radio Show" `shouldBe` Slug "my-radio-show"

    it "collapses double hyphens" $ do
      mkSlug "Hello--World" `shouldBe` Slug "hello-world"

    it "collapses many hyphens" $ do
      mkSlug "Hello-----World" `shouldBe` Slug "hello-world"

    it "converts to lowercase" $ do
      mkSlug "UPPERCASE" `shouldBe` Slug "uppercase"
      mkSlug "MiXeD cAsE" `shouldBe` Slug "mixed-case"

    it "preserves numbers and hyphens" $ do
      mkSlug "Test 123" `shouldBe` Slug "test-123"
      mkSlug "pre-existing-hyphen" `shouldBe` Slug "pre-existing-hyphen"

    it "removes special characters" $ do
      mkSlug "Hello! World?" `shouldBe` Slug "hello-world"
      mkSlug "Show @ KPBJ FM" `shouldBe` Slug "show-kpbj-fm"

    it "handles empty string" $ do
      mkSlug "" `shouldBe` Slug ""

    it "handles only special characters" $ do
      mkSlug "!@#$%^&*()" `shouldBe` Slug ""

    it "only contains valid slug characters" $ do
      let (Slug result) = mkSlug "Hello!-World123"
      Text.all (\c -> c `elem` ("-abcdefghijklmnopqrstuvwxyz0123456789" :: String)) result `shouldBe` True

  describe "Semigroup instance" $ do
    it "combines non-empty slugs with hyphen" $ do
      Slug "hello" <> Slug "world" `shouldBe` Slug "hello-world"

    it "handles empty slugs correctly" $ do
      Slug "hello" <> Slug "" `shouldBe` Slug "hello"
      Slug "" <> Slug "world" `shouldBe` Slug "world"
      Slug "" <> Slug "" `shouldBe` Slug ""
