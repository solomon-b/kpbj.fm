module App.RequestLogSpec (spec) where

--------------------------------------------------------------------------------

import App.RequestLog (redactHeaders, redactQuery, redacted)
import Test.Hspec (Spec, describe, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "App.RequestLog" $ do
  describe "redactHeaders" $ do
    it "redacts the playout secret" $
      redactHeaders [("X-Playout-Secret", "s3cret")]
        `shouldBe` [("X-Playout-Secret", redacted)]

    it "redacts the session cookie" $
      redactHeaders [("Cookie", "session-id=abc123")]
        `shouldBe` [("Cookie", redacted)]

    it "redacts the authorization header" $
      redactHeaders [("Authorization", "Bearer abc123")]
        `shouldBe` [("Authorization", redacted)]

    it "redacts the Stripe signature" $
      redactHeaders [("Stripe-Signature", "t=1,v1=abc123")]
        `shouldBe` [("Stripe-Signature", redacted)]

    it "keeps other headers" $
      redactHeaders [("User-Agent", "liquidsoap"), ("HX-Request", "true")]
        `shouldBe` [("User-Agent", "liquidsoap"), ("HX-Request", "true")]

    -- A request from the password-reset page carries the page URL, and its token,
    -- in the Referer header.
    it "redacts the token in the referer" $
      redactHeaders [("Referer", "https://kpbj.fm/user/reset-password?token=abc123")]
        `shouldBe` [("Referer", "https://kpbj.fm/user/reset-password?token=%3Credacted%3E")]

    it "redacts the token in the HTMX current URL" $
      redactHeaders [("HX-Current-Url", "https://kpbj.fm/user/verify-email?token=abc123&x=1")]
        `shouldBe` [("HX-Current-Url", "https://kpbj.fm/user/verify-email?token=%3Credacted%3E&x=1")]

    it "keeps a referer that has no query" $
      redactHeaders [("Referer", "https://kpbj.fm/shows")]
        `shouldBe` [("Referer", "https://kpbj.fm/shows")]

  describe "redactQuery" $ do
    it "redacts the token" $
      redactQuery [("token", Just "abc123")]
        `shouldBe` [("token", Just redacted)]

    it "keeps other parameters" $
      redactQuery [("page", Just "2"), ("token", Just "abc123"), ("q", Nothing)]
        `shouldBe` [("page", Just "2"), ("token", Just redacted), ("q", Nothing)]
