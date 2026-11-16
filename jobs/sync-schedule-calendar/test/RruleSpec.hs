module RruleSpec (spec) where

--------------------------------------------------------------------------------

import Data.Either (isLeft)
import Data.Time (DayOfWeek (..))
import Domain.Types.Recurrence (everyWeek, recurrenceFromRow, recurring)
import Gen (day, genDay, genRecurrence)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Rrule
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "printRule" $ do
    it "prints a weekly rule" $
      printRule (Rule (recurring Saturday everyWeek) Nothing) `shouldBe` "RRULE:FREQ=WEEKLY;BYDAY=SA"

    it "prints a monthly rule with one ordinal for each week" $
      printRule (Rule (recurrenceFromRow Saturday [1, 3]) Nothing)
        `shouldBe` "RRULE:FREQ=MONTHLY;BYDAY=1SA,3SA"

    it "prints week 5 as the fifth weekday" $
      printRule (Rule (recurrenceFromRow Sunday [5]) Nothing) `shouldBe` "RRULE:FREQ=MONTHLY;BYDAY=5SU"

    it "prints the end of the last day in UTC, in standard time" $
      printRule (Rule (recurring Saturday everyWeek) (Just (day 2026 11 15)))
        `shouldBe` "RRULE:FREQ=WEEKLY;BYDAY=SA;UNTIL=20261116T075959Z"

    it "prints the end of the last day in UTC, in daylight time" $
      printRule (Rule (recurring Saturday everyWeek) (Just (day 2026 7 4)))
        `shouldBe` "RRULE:FREQ=WEEKLY;BYDAY=SA;UNTIL=20260705T065959Z"

  describe "parseRule" $ do
    it "parses what printRule prints" $ hedgehog $ do
      rule <- forAll (Rule <$> genRecurrence <*> Gen.maybe genDay)
      parseRule (printRule rule) === Right rule

    it "accepts the parts in another order, and WKST" $
      parseRule "RRULE:BYDAY=1SA,3SA;WKST=SU;FREQ=MONTHLY"
        `shouldBe` Right (Rule (recurrenceFromRow Saturday [1, 3]) Nothing)

    it "accepts INTERVAL=1" $
      parseRule "RRULE:FREQ=WEEKLY;INTERVAL=1;BYDAY=MO"
        `shouldBe` Right (Rule (recurring Monday everyWeek) Nothing)

    it "reads a monthly rule with all five ordinals as every week" $
      parseRule "RRULE:FREQ=MONTHLY;BYDAY=1TU,2TU,3TU,4TU,5TU"
        `shouldBe` Right (Rule (recurring Tuesday everyWeek) Nothing)

    it "rejects the forms that the job does not write" $
      mapM_
        (\text -> (text, isLeft (parseRule text)) `shouldBe` (text, True))
        [ "FREQ=WEEKLY;BYDAY=SA",
          "RRULE:FREQ=WEEKLY",
          "RRULE:FREQ=WEEKLY;BYDAY=SA;COUNT=10",
          "RRULE:FREQ=WEEKLY;BYDAY=SA,SU",
          "RRULE:FREQ=WEEKLY;BYDAY=1SA",
          "RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=SA",
          "RRULE:FREQ=MONTHLY;BYDAY=SA",
          "RRULE:FREQ=MONTHLY;BYDAY=-1SA",
          "RRULE:FREQ=MONTHLY;BYDAY=1SA,3SU",
          "RRULE:FREQ=MONTHLY;BYMONTHDAY=15",
          "RRULE:FREQ=DAILY",
          "RRULE:FREQ=WEEKLY;BYDAY=SA;UNTIL=20261116",
          "EXDATE:20261116T075959Z"
        ]
