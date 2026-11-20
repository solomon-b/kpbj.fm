{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for "App.FormField".
module App.FormFieldSpec (spec) where

--------------------------------------------------------------------------------

import App.FormField (emailField)
import Test.Gen.EmailAddress (mkEmailAddressUnsafe)
import Test.Hspec (Spec, describe, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "App.FormField" $
    describe "emailField" $ do
      it "accepts a valid address" $
        emailField "host@kpbj.fm" `shouldBe` Right (mkEmailAddressUnsafe "host@kpbj.fm")

      it "changes the address to lower case" $
        emailField "Host@KPBJ.fm" `shouldBe` Right (mkEmailAddressUnsafe "host@kpbj.fm")

      it "removes the spaces around the address" $
        emailField "  host@kpbj.fm \n" `shouldBe` Right (mkEmailAddressUnsafe "host@kpbj.fm")

      it "refuses text that is not an address, with a message for the user" $
        emailField "not-an-email" `shouldBe` Left "Please enter a valid email address."

      it "refuses empty text" $
        emailField "" `shouldBe` Left "Please enter a valid email address."
