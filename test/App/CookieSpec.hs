module App.CookieSpec (spec) where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import App.Cookie (getCookieDomain, mkCookieSessionWithDomain, mkExpireOldSessionCookie)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Effects.Database.Tables.ServerSessions qualified as ServerSessions
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------
-- Generators

-- | Generate any Environment value
genEnvironment :: (MonadGen m) => m Environment
genEnvironment = Gen.element [Development, Staging, Production]

-- | Generate a valid session ID
genSessionId :: (MonadGen m) => m ServerSessions.Id
genSessionId = do
  -- Generate 4 Word32s to build a UUID
  w1 <- Gen.word32 Range.constantBounded
  w2 <- Gen.word32 Range.constantBounded
  w3 <- Gen.word32 Range.constantBounded
  w4 <- Gen.word32 Range.constantBounded
  let uuid = UUID.fromWords w1 w2 w3 w4
  pure $ ServerSessions.Id uuid

--------------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = describe "App.Cookie" $ do
  describe "getCookieDomain" $ do
    it "returns empty string for Development" $ do
      getCookieDomain Development `shouldBe` ""

    it "returns .staging.kpbj.fm for Staging" $ do
      getCookieDomain Staging `shouldBe` ".staging.kpbj.fm"

    it "returns .kpbj.fm for Production" $ do
      getCookieDomain Production `shouldBe` ".kpbj.fm"

  describe "mkCookieSessionWithDomain" $ do
    it "always contains session-id key" $ hedgehog $ do
      env <- forAll genEnvironment
      sessionId <- forAll genSessionId
      let cookie = mkCookieSessionWithDomain env sessionId
      assert $ "session-id=" `Text.isPrefixOf` cookie

    it "always contains SameSite=lax" $ hedgehog $ do
      env <- forAll genEnvironment
      sessionId <- forAll genSessionId
      let cookie = mkCookieSessionWithDomain env sessionId
      assert $ "SameSite=lax" `Text.isInfixOf` cookie

    it "always contains Path=/" $ hedgehog $ do
      env <- forAll genEnvironment
      sessionId <- forAll genSessionId
      let cookie = mkCookieSessionWithDomain env sessionId
      assert $ "Path=/" `Text.isInfixOf` cookie

    it "always contains HttpOnly" $ hedgehog $ do
      env <- forAll genEnvironment
      sessionId <- forAll genSessionId
      let cookie = mkCookieSessionWithDomain env sessionId
      assert $ "HttpOnly" `Text.isInfixOf` cookie

    it "always contains Secure" $ hedgehog $ do
      env <- forAll genEnvironment
      sessionId <- forAll genSessionId
      let cookie = mkCookieSessionWithDomain env sessionId
      assert $ "Secure" `Text.isInfixOf` cookie

    it "Development cookies have no Domain attribute" $ hedgehog $ do
      sessionId <- forAll genSessionId
      let cookie = mkCookieSessionWithDomain Development sessionId
      assert $ not ("Domain=" `Text.isInfixOf` cookie)

    it "Staging cookies have Domain=.staging.kpbj.fm" $ hedgehog $ do
      sessionId <- forAll genSessionId
      let cookie = mkCookieSessionWithDomain Staging sessionId
      assert $ "Domain=.staging.kpbj.fm" `Text.isInfixOf` cookie

    it "Production cookies have Domain=.kpbj.fm" $ hedgehog $ do
      sessionId <- forAll genSessionId
      let cookie = mkCookieSessionWithDomain Production sessionId
      assert $ "Domain=.kpbj.fm" `Text.isInfixOf` cookie

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
