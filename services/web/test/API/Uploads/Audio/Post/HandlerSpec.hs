module API.Uploads.Audio.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Uploads.Audio.Post.Handler (checkUploadLength)
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Test.Hspec (Spec, describe, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "API.Uploads.Audio.Post.Handler.checkUploadLength" $ do
    describe "station IDs" $ do
      it "accepts a file at the 10 second limit" $
        checkUploadLength StagedUploads.StationIdAudio 10 `shouldBe` Right ()
      it "refuses a file one second over the limit" $
        checkUploadLength StagedUploads.StationIdAudio 11
          `shouldBe` Left "Station IDs can run at most 10 seconds. This file runs 11."

    describe "PSAs and underwriting announcements" $ do
      it "accepts a file at the 30 second limit" $
        checkUploadLength StagedUploads.BreakItemAudio 30 `shouldBe` Right ()
      it "refuses a 42 second file" $
        checkUploadLength StagedUploads.BreakItemAudio 42
          `shouldBe` Left "PSAs and underwriting announcements can run at most 30 seconds. This file runs 42."

    describe "uploads with no limit here" $ do
      it "accepts a long episode, because its limit depends on the slot" $
        checkUploadLength StagedUploads.EpisodeAudio 7200 `shouldBe` Right ()
      it "accepts a long ephemeral upload" $
        checkUploadLength StagedUploads.EphemeralAudio 7200 `shouldBe` Right ()
