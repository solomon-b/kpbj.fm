module App.CookieSpec (spec) where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import App.Cookie (lookupSessionId, mkCookieSessionWithDomain, mkExpireOldSessionCookie)
import App.Domains (cookieName)
import Data.Text qualified as Text
import Data.Text.Display (display)
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
  describe "mkCookieSessionWithDomain" $ do
    it "uses environment-specific cookie name" $ hedgehog $ do
      env <- forAll genEnvironment
      sessionId <- forAll genSessionId
      let cookie = mkCookieSessionWithDomain env sessionId
          expectedPrefix = cookieName env <> "="
      assert $ expectedPrefix `Text.isPrefixOf` cookie

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

    it "Staging cookies use session-id-staging name" $ hedgehog $ do
      sessionId <- forAll genSessionId
      let cookie = mkCookieSessionWithDomain Staging sessionId
      assert $ "session-id-staging=" `Text.isPrefixOf` cookie

    it "Production cookies use session-id-production name" $ hedgehog $ do
      sessionId <- forAll genSessionId
      let cookie = mkCookieSessionWithDomain Production sessionId
      assert $ "session-id-production=" `Text.isPrefixOf` cookie

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

  describe "lookupSessionId" $ do
    it "parses session-id cookie for Development" $ hedgehog $ do
      sessionId <- forAll genSessionId
      let cookieHeader = "session-id=" <> display sessionId
      lookupSessionId Development cookieHeader === Just sessionId

    it "parses session-id-production cookie for Production" $ hedgehog $ do
      sessionId <- forAll genSessionId
      let cookieHeader = "session-id-production=" <> display sessionId
      lookupSessionId Production cookieHeader === Just sessionId

    it "parses session-id-staging cookie for Staging" $ hedgehog $ do
      sessionId <- forAll genSessionId
      let cookieHeader = "session-id-staging=" <> display sessionId
      lookupSessionId Staging cookieHeader === Just sessionId

    it "ignores production cookie when looking up staging" $ hedgehog $ do
      sessionId <- forAll genSessionId
      let cookieHeader = "session-id-production=" <> display sessionId
      lookupSessionId Staging cookieHeader === Nothing

    it "ignores staging cookie when looking up production" $ hedgehog $ do
      sessionId <- forAll genSessionId
      let cookieHeader = "session-id-staging=" <> display sessionId
      lookupSessionId Production cookieHeader === Nothing

    it "finds correct cookie when multiple cookies present" $ hedgehog $ do
      stagingId <- forAll genSessionId
      prodId <- forAll genSessionId
      let cookieHeader = "session-id-production=" <> display prodId <> "; session-id-staging=" <> display stagingId
      lookupSessionId Staging cookieHeader === Just stagingId
      lookupSessionId Production cookieHeader === Just prodId
