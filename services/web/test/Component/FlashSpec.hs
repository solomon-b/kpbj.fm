-- | Tests for the redirect helpers and the flash cookie of "Component.Flash".
module Component.FlashSpec (spec) where

--------------------------------------------------------------------------------

import Component.Banner (BannerType (..), bannerTypeFromName, bannerTypeName)
import Component.Flash (FlashMessage (..), decodeFlash, flashCookie, hxLocationValue, hxRedirectError, lookupFlash)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Types.Header (hCookie, hSetCookie)
import Servant qualified
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

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

  describe "bannerTypeFromName" $
    it "is the inverse of bannerTypeName" $
      map (bannerTypeFromName . bannerTypeName) [minBound .. maxBound]
        `shouldBe` map Just [minBound .. maxBound :: BannerType]

  describe "hxRedirectError" $ do
    it "leaves the flash cookie alone when there is no message" $
      redirectCookies Nothing `shouldBe` []

    it "sets the flash cookie when there is a message" $
      redirectCookies (Just created) `shouldBe` [Text.Encoding.encodeUtf8 (flashCookie (Just created))]

  describe "flash cookie" $ do
    it "prop: the request cookie decodes to the message that flashCookie set" $ hedgehog $ do
      flash <- forAll genFlash
      lookupFlash [(hCookie, requestCookie flash)] === Just flash

    it "prop: other cookies in the header do not change the message" $ hedgehog $ do
      flash <- forAll genFlash
      lookupFlash [(hCookie, "session=abc; " <> requestCookie flash <> "; dashboard_show=my-show")] === Just flash

    it "finds the cookie in a second Cookie header" $
      lookupFlash [(hCookie, "session=abc"), (hCookie, requestCookie created)]
        `shouldBe` Just created

    it "is HttpOnly" $
      flashCookie (Just created) `shouldSatisfy` hasAttribute "HttpOnly"

    it "gives Nothing with no flash cookie" $
      lookupFlash [(hCookie, "session=abc")] `shouldBe` Nothing

    it "gives Nothing for a value that is not JSON" $
      decodeFlash "not-json" `shouldBe` Nothing

    it "gives Nothing for an unknown banner type" $
      decodeFlash "%7B%22type%22%3A%22party%22%2C%22title%22%3A%22a%22%2C%22msg%22%3A%22b%22%7D"
        `shouldBe` Nothing

--------------------------------------------------------------------------------

-- | Decode a header value as JSON.
decodeValue :: Text -> Maybe Aeson.Value
decodeValue = Aeson.decodeStrict . Text.Encoding.encodeUtf8

-- | The JSON object that HTMX expects in an @HX-Location@ header.
location :: Text -> Text -> Text -> Aeson.Value
location path target swap =
  Aeson.object ["path" .= path, "target" .= target, "swap" .= swap]

-- | A flash message.
created :: FlashMessage
created = FlashMessage Success "Created" "Item created successfully."

-- | Flash messages with text that needs encoding: spaces, quotes,
-- semicolons, equals signs, percent signs, and non-ASCII characters.
genFlash :: Gen FlashMessage
genFlash =
  FlashMessage
    <$> Gen.enumBounded
    <*> Gen.text (Range.linear 0 40) Gen.unicode
    <*> Gen.text (Range.linear 0 80) Gen.unicode

-- | The @name=value@ pair that a browser sends back for the cookie that
-- 'flashCookie' sets. It is the part of the @Set-Cookie@ value before the
-- first attribute.
requestCookie :: FlashMessage -> ByteString
requestCookie =
  Char8.takeWhile (/= ';') . Text.Encoding.encodeUtf8 . flashCookie . Just

-- | Whether a @Set-Cookie@ value has an attribute.
hasAttribute :: ByteString -> Text -> Bool
hasAttribute attribute setCookie =
  ("; " <> attribute) `ByteString.isInfixOf` Text.Encoding.encodeUtf8 setCookie

-- | The @Set-Cookie@ values of the redirect that 'hxRedirectError' builds.
redirectCookies :: Maybe FlashMessage -> [ByteString]
redirectCookies mFlash =
  [value | (name, value) <- Servant.errHeaders (hxRedirectError "/dashboard" mFlash), name == hSetCookie]
