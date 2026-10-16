module Effects.Storage.StagingSpec (spec) where

--------------------------------------------------------------------------------

import Effects.Storage.Staging (isStagingPath, stagingPrefix)
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "stagingPrefix" $
    it "is temp/staging/" $
      stagingPrefix `shouldBe` "temp/staging/"

  describe "isStagingPath" $ do
    it "accepts a file in the staging prefix" $
      isStagingPath "temp/staging/abc123.mp3" `shouldBe` True

    it "refuses an episode archive path" $
      isStagingPath "audio/episodes/2026/10/15/show_2026-10-15_abc123.mp3" `shouldBe` False

    it "refuses an image archive path" $
      isStagingPath "images/logos/2026/10/15/show_abc123.png" `shouldBe` False

    it "refuses a path in temp/ outside staging/" $
      isStagingPath "temp/other/abc123.mp3" `shouldBe` False

    it "refuses the prefix with no file name" $
      isStagingPath "temp/staging/" `shouldBe` False

    it "refuses a path that climbs out of the prefix" $
      isStagingPath "temp/staging/../../audio/episodes/x.mp3" `shouldBe` False

    it "refuses a path that starts with a slash" $
      isStagingPath "/temp/staging/abc123.mp3" `shouldBe` False

    it "refuses an empty path" $
      isStagingPath "" `shouldBe` False
