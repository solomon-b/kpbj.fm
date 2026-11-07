-- | Tests for the episode length limits.
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

    describe "minEpisodeSeconds" $ do
      it "allows one minute of a 1 hour slot before the break" $
        minEpisodeSeconds (hour 10) (hour 11) `shouldBe` 3420

    describe "checkEpisodeFitsSlot" $ do
      it "accepts a 30 minute slot episode from 27:00 to 28:00 only" $ do
        let check = checkEpisodeFitsSlot (hour 10) (TimeOfDay 10 30 0)
        check 1619 `shouldSatisfy` isLeft
        check 1620 `shouldBe` Right ()
        check 1680 `shouldBe` Right ()
        check 1681 `shouldSatisfy` isLeft

      it "accepts a 1 hour slot episode from 57:00 to 58:00 only" $ do
        let check = checkEpisodeFitsSlot (hour 10) (hour 11)
        check 3419 `shouldSatisfy` isLeft
        check 3420 `shouldBe` Right ()
        check 3480 `shouldBe` Right ()
        check 3481 `shouldSatisfy` isLeft

      it "accepts a 1 hour 30 minute slot episode from 1:27:00 to 1:28:00 only" $ do
        let check = checkEpisodeFitsSlot (hour 10) (TimeOfDay 11 30 0)
        check 5219 `shouldSatisfy` isLeft
        check 5220 `shouldBe` Right ()
        check 5280 `shouldBe` Right ()
        check 5281 `shouldSatisfy` isLeft

      it "accepts a 2 hour slot episode from 1:57:00 to 1:58:00 only" $ do
        let check = checkEpisodeFitsSlot (hour 14) (hour 16)
        check 7019 `shouldSatisfy` isLeft
        check 7020 `shouldBe` Right ()
        check 7080 `shouldBe` Right ()
        check 7081 `shouldSatisfy` isLeft

      it "measures a slot from 23:00 to 01:00 as two hours" $ do
        let check = checkEpisodeFitsSlot (hour 23) (hour 1)
        check 7019 `shouldSatisfy` isLeft
        check 7020 `shouldBe` Right ()
        check 7080 `shouldBe` Right ()
        check 7081 `shouldSatisfy` isLeft

      it "tells the host the length, both limits, and what to do with a long episode" $
        checkEpisodeFitsSlot (hour 10) (hour 11) 3552
          `shouldBe` Left "This episode runs 59:12. Episodes in this 1 hour slot must run from 57:00 to 58:00, so the station break fits. Trim the audio and upload it again."

      it "tells the host the length, both limits, and what to do with a short episode" $
        checkEpisodeFitsSlot (hour 10) (hour 11) 1200
          `shouldBe` Left "This episode runs 20:00. Episodes in this 1 hour slot must run from 57:00 to 58:00, so the episode fills the slot up to the station break. Add audio and upload it again."

      it "names a 30 minute slot in minutes" $
        checkEpisodeFitsSlot (hour 10) (TimeOfDay 10 30 0) 1800
          `shouldBe` Left "This episode runs 30:00. Episodes in this 30 minute slot must run from 27:00 to 28:00, so the station break fits. Trim the audio and upload it again."

      it "names a 90 minute slot in hours and minutes" $
        checkEpisodeFitsSlot (hour 10) (TimeOfDay 11 30 0) 5400
          `shouldBe` Left "This episode runs 1:30:00. Episodes in this 1 hour 30 minute slot must run from 1:27:00 to 1:28:00, so the station break fits. Trim the audio and upload it again."

    describe "formatLength" $ do
      it "formats a length under one hour as M:SS" $ do
        formatLength 0 `shouldBe` "0:00"
        formatLength 65 `shouldBe` "1:05"
        formatLength 3552 `shouldBe` "59:12"

      it "formats a length of one hour or more as H:MM:SS" $ do
        formatLength 3600 `shouldBe` "1:00:00"
        formatLength 7081 `shouldBe` "1:58:01"
