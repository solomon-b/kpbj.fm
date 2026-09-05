module Domain.Types.TimezoneSpec (spec) where

--------------------------------------------------------------------------------

import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Domain.Types.Timezone (pacificDay)
import Test.Hspec (Spec, describe, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "pacificDay" $ do
    it "reads the Pacific date, not the UTC date" $
      -- 04:00 UTC is 21:00 the previous evening in Pacific. This is the case
      -- every hand-rolled copy of this conversion existed to get right.
      pacificDay (UTCTime (fromGregorian 2026 3 10) (secondsToDiffTime (4 * 3600)))
        `shouldBe` fromGregorian 2026 3 9

    it "agrees with the UTC date earlier in the Pacific day" $
      -- 17:00 UTC is 10:00 the same morning in Pacific.
      pacificDay (UTCTime (fromGregorian 2026 3 9) (secondsToDiffTime (17 * 3600)))
        `shouldBe` fromGregorian 2026 3 9

    it "reads the date across the spring-forward transition" $
      -- 2026-03-08 is the spring-forward date. 12:00 UTC is 05:00 PDT.
      pacificDay (UTCTime (fromGregorian 2026 3 8) (secondsToDiffTime (12 * 3600)))
        `shouldBe` fromGregorian 2026 3 8

    it "reads the date across the fall-back transition" $
      -- 2026-11-01 is the fall-back date. 12:00 UTC is 04:00 PST.
      pacificDay (UTCTime (fromGregorian 2026 11 1) (secondsToDiffTime (12 * 3600)))
        `shouldBe` fromGregorian 2026 11 1
