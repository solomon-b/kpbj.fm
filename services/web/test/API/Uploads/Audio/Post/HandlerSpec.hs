module API.Uploads.Audio.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Uploads.Audio.Post.Handler (checkUploadLength)
import Data.Either (isLeft)
import Data.Text (Text)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Servant qualified
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "API.Uploads.Audio.Post.Handler.checkUploadLength" $ do
    describe "station IDs" $ do
      it "accepts a file at the 10 second limit" $
        checkUploadLength StagedUploads.StationIdAudio Nothing 10 `shouldBe` Right ()
      it "refuses a file one second over the limit" $
        checkUploadLength StagedUploads.StationIdAudio Nothing 11
          `shouldBe` Left "Station IDs can run at most 10 seconds. This file runs 11."

    describe "break items with no category" $ do
      it "accepts a file at the 30 second limit" $
        checkUploadLength StagedUploads.BreakItemAudio Nothing 30 `shouldBe` Right ()
      it "refuses a 42 second file" $
        checkUploadLength StagedUploads.BreakItemAudio Nothing 42
          `shouldBe` Left "Break items can run at most 30 seconds. This file runs 42."

    describe "break items with a category" $ do
      it "accepts a show promo at the 30 second limit" $
        checkUploadLength StagedUploads.BreakItemAudio (Just BreakItems.ShowPromo) 30 `shouldBe` Right ()
      it "refuses a show promo one second over the limit" $
        checkUploadLength StagedUploads.BreakItemAudio (Just BreakItems.ShowPromo) 31
          `shouldBe` Left "Show promos can run at most 30 seconds. This file runs 31."
      it "accepts a PSA at the 30 second limit" $
        checkUploadLength StagedUploads.BreakItemAudio (Just BreakItems.Psa) 30 `shouldBe` Right ()

    describe "uploads with no limit here" $ do
      it "accepts a long episode, because its limit depends on the slot" $
        checkUploadLength StagedUploads.EpisodeAudio Nothing 7200 `shouldBe` Right ()
      it "accepts a long ephemeral upload" $
        checkUploadLength StagedUploads.EphemeralAudio Nothing 7200 `shouldBe` Right ()

    describe "the category query parameter" $ do
      it "parses what it renders for every category" $
        map (Servant.parseUrlPiece . Servant.toUrlPiece) [minBound .. maxBound :: BreakItems.Category]
          `shouldBe` map Right [minBound .. maxBound :: BreakItems.Category]
      it "refuses an unknown category" $
        (Servant.parseUrlPiece "jingle" :: Either Text BreakItems.Category) `shouldSatisfy` isLeft
