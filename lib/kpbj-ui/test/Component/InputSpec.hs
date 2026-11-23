-- | Render tests for the boxed inputs: the text input, the textarea, and the
-- select.
module Component.InputSpec (spec) where

--------------------------------------------------------------------------------

import Component.Input qualified as Input
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Component.Input" $ do
  it "renders an input with the old .fb-input classes and its attributes" $
    render (Input.input [Lucid.type_ "email", Lucid.name_ "email"])
      `shouldBe` ("<input class=\"" <> Text.unwords inputClasses <> "\" type=\"email\" name=\"email\">")

  it "renders a textarea with the old .fb-textarea classes" $
    render (Input.textarea [Lucid.name_ "body"] "text")
      `shouldBe` ("<textarea class=\"" <> Text.unwords textareaClasses <> "\" name=\"body\">text</textarea>")

  it "renders a select with the old .fb-select classes" $
    render (Input.select [Lucid.name_ "kind"] (Lucid.option_ "A"))
      `shouldBe` ("<select class=\"" <> Text.unwords selectClasses <> "\" name=\"kind\"><option>A</option></select>")

  it "toggles the error class with an Alpine condition" $
    render (Input.input [Input.errorWhen "showErrors && !fields.name.isValid"])
      `shouldSatisfy` Text.isInfixOf "x-bind:class=\"{ &#39;!border-[var(--theme-error)]&#39;: showErrors &amp;&amp; !fields.name.isValid }\""

--------------------------------------------------------------------------------

-- | The classes that the input, the textarea, and the select share.
boxClasses :: [Text]
boxClasses =
  [ "w-full",
    "p-3",
    "border",
    "border-[var(--theme-border)]",
    "bg-[var(--theme-bg)]",
    "text-[var(--theme-fg)]",
    "[font-family:ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace]",
    "transition-[border-color]",
    "duration-150",
    "ease-[ease-in-out]",
    "focus:border-[var(--theme-info)]",
    "focus:[outline:none]"
  ]

inputClasses :: [Text]
inputClasses =
  boxClasses
    <> [ "disabled:bg-[var(--theme-bg-alt)]",
         "disabled:text-[var(--theme-fg-muted)]",
         "disabled:cursor-not-allowed"
       ]

textareaClasses :: [Text]
textareaClasses = boxClasses <> ["leading-relaxed", "resize-y"]

selectClasses :: [Text]
selectClasses = boxClasses <> ["cursor-pointer", "disabled:cursor-default"]

render :: Lucid.Html () -> Text
render = LText.toStrict . Lucid.renderText
