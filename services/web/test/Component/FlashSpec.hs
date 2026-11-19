-- | Tests for the redirect helpers of "Component.Flash".
module Component.FlashSpec (spec) where

--------------------------------------------------------------------------------

import Component.Flash (hxLocationValue)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Component.Flash" $ do
  describe "hxLocationValue" $ do
    it "is a JSON object with the path, the target, and the swap" $
      decodeValue (hxLocationValue "/dashboard/episodes/show-a" "#main-content" "innerHTML show:window:top")
        `shouldBe` Just (location "/dashboard/episodes/show-a" "#main-content" "innerHTML show:window:top")

    it "keeps a quote in the path inside the JSON string" $
      decodeValue (hxLocationValue "/a\"b" "#main-content" "innerHTML")
        `shouldBe` Just (location "/a\"b" "#main-content" "innerHTML")

--------------------------------------------------------------------------------

-- | Decode a header value as JSON.
decodeValue :: Text -> Maybe Aeson.Value
decodeValue = Aeson.decodeStrict . Text.Encoding.encodeUtf8

-- | The JSON object that HTMX expects in an @HX-Location@ header.
location :: Text -> Text -> Text -> Aeson.Value
location path target swap =
  Aeson.object ["path" .= path, "target" .= target, "swap" .= swap]
