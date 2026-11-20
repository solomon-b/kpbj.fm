{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for "App.ErrorFormatters".
--
-- Pure tests for the response that Servant sends when a request body does not
-- decode. No database or AppM required.
module App.ErrorFormattersSpec (spec) where

--------------------------------------------------------------------------------

import App.ErrorFormatters (errorFormatters)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Proxy (Proxy (..))
import Data.Typeable (typeRep)
import Network.Wai qualified as Wai
import Servant qualified
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "App.ErrorFormatters" $
    describe "bodyParserErrorFormatter" $ do
      it "answers an HTMX request with a 200 and the message in a banner" $ do
        let response = format htmxRequest "Please enter a valid email address."
            body = BSL.toStrict (Servant.errBody response)
        Servant.errHTTPCode response `shouldBe` 200
        body `shouldSatisfy` ("banner-container" `BS.isInfixOf`)
        body `shouldSatisfy` ("Please enter a valid email address." `BS.isInfixOf`)

      it "leaves the form of an HTMX request on the page" $
        Servant.errHeaders (format htmxRequest "nope")
          `shouldContain` [("HX-Reswap", "none show:window:top")]

      it "answers a JSON request with a 400 and the message in an error field" $ do
        let response = format jsonRequest "nope"
        Servant.errHTTPCode response `shouldBe` 400
        Servant.errBody response `shouldBe` "{\"error\":\"nope\"}"
        Servant.errHeaders response `shouldContain` [("Content-Type", "application/json")]

      it "answers every other request with the default 400" $ do
        let response = format Wai.defaultRequest "nope"
        Servant.errHTTPCode response `shouldBe` 400
        Servant.errBody response `shouldBe` "nope"

--------------------------------------------------------------------------------
-- Helpers

format :: Wai.Request -> String -> Servant.ServerError
format = Servant.bodyParserErrorFormatter errorFormatters (typeRep (Proxy :: Proxy ()))

htmxRequest :: Wai.Request
htmxRequest = Wai.defaultRequest {Wai.requestHeaders = [("HX-Request", "true")]}

jsonRequest :: Wai.Request
jsonRequest = Wai.defaultRequest {Wai.requestHeaders = [("Content-Type", "application/json")]}
