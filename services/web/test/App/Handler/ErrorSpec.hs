{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for "App.Handler.Error".
--
-- Pure tests for the response builders that turn handler errors into HTMX
-- responses. No database or AppM required.
module App.Handler.ErrorSpec (spec) where

--------------------------------------------------------------------------------

import App.Handler.Error (HandlerError (..), inlineBannerError)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Servant qualified
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "App.Handler.Error" $
    describe "inlineBannerError" $ do
      it "returns a 200 response for a validation error" $
        Servant.errHTTPCode (inlineBannerError (ValidationError "nope")) `shouldBe` 200

      it "sets HX-Reswap: none so the form target is not blanked" $
        Servant.errHeaders (inlineBannerError (ValidationError "nope"))
          `shouldContain` [("HX-Reswap", "none show:window:top")]

      it "still carries the OOB banner body" $ do
        let body = BSL.toStrict (Servant.errBody (inlineBannerError (ValidationError "nope")))
        body `shouldSatisfy` ("banner-container" `BS.isInfixOf`)
        body `shouldSatisfy` ("nope" `BS.isInfixOf`)

      it "sets HX-Reswap: none for a not-found error too" $
        Servant.errHeaders (inlineBannerError (NotFound "Thing"))
          `shouldContain` [("HX-Reswap", "none show:window:top")]
