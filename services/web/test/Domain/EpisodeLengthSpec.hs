-- | Tests for the episode length cap.
module Domain.EpisodeLengthSpec where

--------------------------------------------------------------------------------

import Data.Either (isLeft)
import Data.Time (TimeOfDay (..))
import Domain.EpisodeLength
import Test.Hspec

--------------------------------------------------------------------------------

-- | A whole-hour time of day.
hour :: Int -> TimeOfDay
hour h = TimeOfDay h 0 0

spec :: Spec
spec =
  describe "Domain.EpisodeLength" $ do
    describe "slotSeconds" $ do
      it "measures a slot inside one day" $
        slotSeconds (hour 10) (hour 11) `shouldBe` 3600

      it "measures a slot that runs past midnight" $
        slotSeconds (hour 23) (hour 1) `shouldBe` 7200

      it "measures a slot that ends at midnight" $
        slotSeconds (hour 23) (hour 0) `shouldBe` 3600

    describe "maxEpisodeSeconds" $ do
      it "leaves two minutes of a 1 hour slot for the break" $
        maxEpisodeSeconds (hour 10) (hour 11) `shouldBe` 3480

    describe "checkEpisodeFitsSlot" $ do
      it "accepts a 1 hour slot episode at exactly 58:00" $
        checkEpisodeFitsSlot (hour 10) (hour 11) 3480 `shouldBe` Right ()

      it "rejects a 1 hour slot episode one second over 58:00" $
        checkEpisodeFitsSlot (hour 10) (hour 11) 3481 `shouldSatisfy` isLeft

      it "accepts a 30 minute slot episode at 28:00 and rejects 28:01" $ do
        checkEpisodeFitsSlot (hour 10) (TimeOfDay 10 30 0) 1680 `shouldBe` Right ()
        checkEpisodeFitsSlot (hour 10) (TimeOfDay 10 30 0) 1681 `shouldSatisfy` isLeft

      it "accepts a 2 hour slot episode at 1:58:00 and rejects 1:58:01" $ do
        checkEpisodeFitsSlot (hour 14) (hour 16) 7080 `shouldBe` Right ()
        checkEpisodeFitsSlot (hour 14) (hour 16) 7081 `shouldSatisfy` isLeft

      it "measures a slot from 23:00 to 01:00 as two hours" $ do
        checkEpisodeFitsSlot (hour 23) (hour 1) 7080 `shouldBe` Right ()
        checkEpisodeFitsSlot (hour 23) (hour 1) 7081 `shouldSatisfy` isLeft

      it "tells the host the length, the cap, and what to do" $
        checkEpisodeFitsSlot (hour 10) (hour 11) 3552
          `shouldBe` Left "This episode runs 59:12. Episodes in this 1 hour slot can run at most 58:00, so the station break fits. Trim the audio and upload it again."

      it "names a 30 minute slot in minutes" $
        checkEpisodeFitsSlot (hour 10) (TimeOfDay 10 30 0) 1800
          `shouldBe` Left "This episode runs 30:00. Episodes in this 30 minute slot can run at most 28:00, so the station break fits. Trim the audio and upload it again."

      it "names a 90 minute slot in hours and minutes" $
        checkEpisodeFitsSlot (hour 10) (TimeOfDay 11 30 0) 5400
          `shouldBe` Left "This episode runs 1:30:00. Episodes in this 1 hour 30 minute slot can run at most 1:28:00, so the station break fits. Trim the audio and upload it again."

    describe "formatLength" $ do
      it "formats a length under one hour as M:SS" $ do
        formatLength 0 `shouldBe` "0:00"
        formatLength 65 `shouldBe` "1:05"
        formatLength 3552 `shouldBe` "59:12"

      it "formats a length of one hour or more as H:MM:SS" $ do
        formatLength 3600 `shouldBe` "1:00:00"
        formatLength 7081 `shouldBe` "1:58:01"
