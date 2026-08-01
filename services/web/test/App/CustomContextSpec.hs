module App.CustomContextSpec where

--------------------------------------------------------------------------------

import App.CustomContext (PlayoutSecret (..), mkPlayoutSecret, secretsMatch)
import Test.Hspec (Spec, describe, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "App.CustomContext" $ do
  describe "mkPlayoutSecret" $ do
    it "treats an unset variable as absent" $
      unPlayoutSecret (mkPlayoutSecret Nothing) `shouldBe` Nothing

    -- A blank value passed straight through before this fix. The /played handler
    -- compares the configured secret with the X-Playout-Secret header. A blank secret
    -- therefore authenticated any caller who sent a blank header.
    it "treats a blank value as absent" $
      unPlayoutSecret (mkPlayoutSecret (Just "")) `shouldBe` Nothing

    it "treats a whitespace-only value as absent" $
      unPlayoutSecret (mkPlayoutSecret (Just "   ")) `shouldBe` Nothing

    it "keeps a real secret" $
      unPlayoutSecret (mkPlayoutSecret (Just "s3cret")) `shouldBe` Just "s3cret"

    -- The loader does not trim the value. A secret can begin or end with a space, and
    -- the header must match it exactly.
    it "does not trim a secret that has surrounding whitespace" $
      unPlayoutSecret (mkPlayoutSecret (Just " s3cret ")) `shouldBe` Just " s3cret "

  describe "secretsMatch" $ do
    it "accepts an identical secret" $
      secretsMatch "s3cret" "s3cret" `shouldBe` True

    it "rejects a different secret of the same length" $
      secretsMatch "s3cretX" "s3cretY" `shouldBe` False

    -- A correct prefix must not match. Plain equality also rejected it, but the time
    -- it took grew with the length of the prefix.
    it "rejects a correct prefix" $
      secretsMatch "s3c" "s3cret" `shouldBe` False

    it "rejects a longer string that starts with the secret" $
      secretsMatch "s3cretplus" "s3cret" `shouldBe` False

    it "rejects an empty presented secret" $
      secretsMatch "" "s3cret" `shouldBe` False

    it "is case sensitive" $
      secretsMatch "S3CRET" "s3cret" `shouldBe` False

    it "handles multi-byte characters" $
      secretsMatch "sécret" "sécret" `shouldBe` True
