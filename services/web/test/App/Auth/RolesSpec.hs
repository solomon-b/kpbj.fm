{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for "App.Auth.Roles".
--
-- Pure tests for the change of an auth failure into the login redirect. No
-- database required. "App.Auth.GateSpec" tests the gate with real sessions.
module App.Auth.RolesSpec (spec) where

--------------------------------------------------------------------------------

import API.Links (rootLink, userLinks)
import API.Types (UserRoutes (..))
import App.Auth.Roles (toLoginRedirect)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Data.Text.Encoding (encodeUtf8)
import Servant qualified
import Test.Hspec (Spec, describe, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "App.Auth.Roles" $
    describe "toLoginRedirect" $ do
      it "turns a 401 into the login redirect" $
        shouldBeLoginRedirect (toLoginRedirect Servant.err401)

      it "turns a 307 into the login redirect" $
        shouldBeLoginRedirect (toLoginRedirect Servant.err307)

      it "keeps an error that is not a 401 or a 307" $
        toLoginRedirect Servant.err403 `shouldBe` Servant.err403

--------------------------------------------------------------------------------
-- Helpers

shouldBeLoginRedirect :: Servant.ServerError -> IO ()
shouldBeLoginRedirect e = do
  Servant.errHTTPCode e `shouldBe` 200
  lookup "HX-Redirect" (Servant.errHeaders e)
    `shouldBe` Just (encodeUtf8 (rootLink (userLinks.loginGet Nothing Nothing)))
  lookup "Set-Cookie" (Servant.errHeaders e)
    `shouldBe` Just (encodeUtf8 (flashCookie (Just loginFlash)))

loginFlash :: FlashMessage
loginFlash = FlashMessage Error "Login Required" "Please log in to continue."
