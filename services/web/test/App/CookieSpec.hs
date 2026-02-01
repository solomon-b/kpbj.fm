module App.CookieSpec (spec) where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import App.Cookie (mkExpireOldSessionCookie)
import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = describe "App.Cookie" $ do
  describe "mkExpireOldSessionCookie" $ do
    it "returns Nothing for Development" $ do
      mkExpireOldSessionCookie Development `shouldBe` Nothing

    it "returns Just for Staging" $ do
      mkExpireOldSessionCookie Staging `shouldSatisfy` (/= Nothing)

    it "returns Just for Production" $ do
      mkExpireOldSessionCookie Production `shouldSatisfy` (/= Nothing)

    it "expiration cookie contains Max-Age=0" $ hedgehog $ do
      env <- forAll $ Gen.element [Staging, Production]
      case mkExpireOldSessionCookie env of
        Nothing -> failure
        Just cookie -> assert $ "Max-Age=0" `Text.isInfixOf` cookie

    it "expiration cookie contains session-id key" $ hedgehog $ do
      env <- forAll $ Gen.element [Staging, Production]
      case mkExpireOldSessionCookie env of
        Nothing -> failure
        Just cookie -> assert $ "session-id=" `Text.isPrefixOf` cookie

    it "expiration cookie has NO Domain attribute (to expire old cookies)" $ hedgehog $ do
      env <- forAll $ Gen.element [Staging, Production]
      case mkExpireOldSessionCookie env of
        Nothing -> failure
        Just cookie -> assert $ not ("Domain=" `Text.isInfixOf` cookie)
