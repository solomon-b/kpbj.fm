-- | Render tests for the checkbox and the radio buttons.
module Component.ChoiceSpec (spec) where

--------------------------------------------------------------------------------

import Component.Choice (Requirement (..))
import Component.Choice qualified as Choice
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Component.Choice" $ do
  describe "checkbox" $ do
    it "renders the old .fb-checkbox structure and classes" $
      render (Choice.checkbox "agree" [Lucid.name_ "agree"] "Agree" Optional Nothing)
        `shouldBe` mconcat
          [ "<div class=\"flex items-start gap-2\">",
            "<input type=\"checkbox\" id=\"agree\" class=\"mt-1 cursor-pointer disabled:cursor-default\" name=\"agree\">",
            "<label for=\"agree\" class=\"cursor-pointer text-[var(--theme-fg)]\">",
            "<span class=\"font-medium\">Agree</span>",
            "</label></div>"
          ]

    it "renders the required mark for a required checkbox" $
      render (Choice.checkbox "agree" [] "Agree" Required Nothing)
        `shouldSatisfy` Text.isInfixOf "<span class=\"font-medium\">Agree<span class=\"text-[var(--theme-error)] ml-1\">*</span></span>"

    it "renders no required mark for an optional checkbox" $
      render (Choice.checkbox "agree" [] "Agree" Optional Nothing)
        `shouldNotSatisfy` Text.isInfixOf "*"

    it "renders the description under the label text" $
      render (Choice.checkbox "agree" [] "Agree" Optional (Just "Terms"))
        `shouldSatisfy` Text.isInfixOf "</span><div class=\"text-[0.875rem] text-[var(--theme-fg-muted)] mt-1\">Terms</div></label>"

  describe "radio" $ do
    it "renders the group with the old .fb-radio-group classes" $
      render (Choice.radioGroup [Lucid.id_ "g"] "x")
        `shouldBe` "<div class=\"flex flex-col gap-2\" id=\"g\">x</div>"

    it "toggles the old .fb-radio-group--error look with an Alpine condition" $
      render (Choice.radioGroup [Choice.radioErrorWhen "bad"] "x")
        `shouldSatisfy` Text.isInfixOf "x-bind:class=\"{ &#39;p-2 -m-2 border border-[var(--theme-error)]&#39;: bad }\""

    it "renders an option with the old .fb-radio-* classes" $
      render (Choice.radioOption "c-x" [Lucid.name_ "c"] "X" (Just "The x"))
        `shouldBe` mconcat
          [ "<label for=\"c-x\" class=\"flex items-start gap-2 cursor-pointer text-[var(--theme-fg)]\">",
            "<input type=\"radio\" id=\"c-x\" class=\"mt-1 cursor-pointer disabled:cursor-default\" name=\"c\">",
            "<span class=\"font-medium\">X</span>",
            "<span class=\"text-[0.875rem] text-[var(--theme-fg-muted)] ml-6\">The x</span>",
            "</label>"
          ]

--------------------------------------------------------------------------------

render :: Lucid.Html () -> Text
render = LText.toStrict . Lucid.renderText
