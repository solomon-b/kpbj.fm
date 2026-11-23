-- | Render tests for the modal.
module Component.ModalSpec (spec) where

--------------------------------------------------------------------------------

import Component.Modal (Look (..))
import Component.Modal qualified as Modal
import Data.Text (Text)
import Data.Text.Lazy qualified as LText
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Component.Modal" $ do
  it "renders the standard look with the old password modal classes" $
    render (Modal.modal Standard "open" "open = false" [Lucid.id_ "m"] "body")
      `shouldBe` mconcat
        [ "<div class=\"fixed inset-0 z-50 flex items-center justify-center p-4\" x-show=\"open\" id=\"m\">",
          "<div class=\"absolute inset-0 bg-black opacity-70\" x-on:click=\"open = false\"></div>",
          "<div class=\"relative w-full max-w-md bg-[var(--theme-bg)] text-[var(--theme-fg)] border-2 border-[var(--theme-border)] p-6\">",
          "body",
          "</div></div>"
        ]

  it "renders the cropper look with utilities for the old crop modal rules" $
    render (Modal.modal Cropper "showCropper" "cancelCrop()" [Lucid.style_ "display: none;"] "body")
      `shouldBe` mconcat
        [ "<div class=\"fixed inset-0 z-50 flex items-center justify-center\" x-show=\"showCropper\" style=\"display: none;\">",
          "<div class=\"absolute inset-0 bg-black/75\" x-on:click=\"cancelCrop()\"></div>",
          "<div class=\"relative bg-[var(--theme-bg)] border border-[var(--theme-border)] w-[90%] max-w-[600px] max-h-[90vh] overflow-hidden flex flex-col\">",
          "body",
          "</div></div>"
        ]

--------------------------------------------------------------------------------

render :: Lucid.Html () -> Text
render = LText.toStrict . Lucid.renderText
