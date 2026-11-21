{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for "App.Auth.Roles".
--
-- Pure tests for the change of an auth failure into the login redirect or a
-- plain 401. No database required. "App.Auth.GateSpec" tests the gate with real sessions.
module App.Auth.RolesSpec (spec) where

--------------------------------------------------------------------------------

import API.Links (rootLink, userLinks)
import API.Types (UserRoutes (..))
import App.Auth.Roles (rendersHtml, toLoginRedirect)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Header)
import Network.Wai qualified as Wai
import Servant qualified
import Test.Hspec (Spec, describe, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "App.Auth.Roles" $ do
    describe "toLoginRedirect" $ do
      it "turns a 401 into the login redirect for HTMX" $
        shouldBeLoginRedirect (toLoginRedirect htmxRequest Servant.err401)

      it "turns a 307 into the login redirect for HTMX" $
        shouldBeLoginRedirect (toLoginRedirect htmxRequest Servant.err307)

      it "turns a 307 into the login redirect for a browser page load" $
        shouldBeLoginRedirect (toLoginRedirect browserRequest Servant.err307)

      it "turns a 401 into a plain 401 for a client that does not render HTML" $
        toLoginRedirect fetchRequest Servant.err401 `shouldBe` Servant.err401

      it "turns a 307 into a plain 401 for a client that does not render HTML" $
        toLoginRedirect fetchRequest Servant.err307 `shouldBe` Servant.err401

      it "keeps an error that is not a 401 or a 307" $
        toLoginRedirect htmxRequest Servant.err403 `shouldBe` Servant.err403

    describe "rendersHtml" $ do
      it "is true for an HTMX request" $
        rendersHtml htmxRequest `shouldBe` True

      it "is true for a request that accepts text/html" $
        rendersHtml browserRequest `shouldBe` True

      it "is false for a request that accepts any type" $
        rendersHtml fetchRequest `shouldBe` False

      it "is false for a request that accepts JSON" $
        rendersHtml (withHeaders [("Accept", "application/json")]) `shouldBe` False

      it "is false for a request with no headers" $
        rendersHtml (withHeaders []) `shouldBe` False

--------------------------------------------------------------------------------
-- Helpers

withHeaders :: [Header] -> Wai.Request
withHeaders headers = Wai.defaultRequest {Wai.requestHeaders = headers}

-- | A request that HTMX sends.
htmxRequest :: Wai.Request
htmxRequest = withHeaders [("HX-Request", "true"), ("Accept", "*/*")]

-- | A page load in a browser.
browserRequest :: Wai.Request
browserRequest = withHeaders [("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")]

-- | A @fetch@ call with the default headers of the browser.
fetchRequest :: Wai.Request
fetchRequest = withHeaders [("Accept", "*/*")]

shouldBeLoginRedirect :: Servant.ServerError -> IO ()
shouldBeLoginRedirect e = do
  Servant.errHTTPCode e `shouldBe` 200
  lookup "HX-Redirect" (Servant.errHeaders e)
    `shouldBe` Just (encodeUtf8 (rootLink (userLinks.loginGet Nothing Nothing)))
  lookup "Set-Cookie" (Servant.errHeaders e)
    `shouldBe` Just (encodeUtf8 (flashCookie (Just loginFlash)))

loginFlash :: FlashMessage
loginFlash = FlashMessage Error "Login Required" "Please log in to continue."
