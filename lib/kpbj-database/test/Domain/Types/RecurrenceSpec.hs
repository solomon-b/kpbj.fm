-- | Property tests for 'Domain.Types.Recurrence'.
--
-- The point of these is the round trip. A recurrence is written to
-- @schedule_templates@ as a weekday and an array of week numbers, read back to
-- populate the schedule editor, and posted again by the form. Every step of that
-- path has to preserve the recurrence, or a show's schedule changes because someone
-- opened a form.
module Domain.Types.RecurrenceSpec (spec) where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (DayOfWeek (..))
import Domain.Types.Recurrence
  ( Recurrence,
    airsEveryWeek,
    everyWeek,
    formatRecurrence,
    frequencyLabel,
    mkWeekOfMonth,
    parseRecurrence,
    parseWeeks,
    recurrenceDay,
    recurrenceFromRow,
    recurrenceWeeks,
    editorCanShow,
    editorWeekSets,
    recurring,
    weekNumber,
    weekNumbers,
    weeksLabel,
  )
import Hedgehog (Gen, assert, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import OrphanInstances.DayOfWeek (dayOfWeekToPostgres)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

-- | Any weekday.
genDay :: Gen DayOfWeek
genDay = Gen.element [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]

-- | Any week set the @weeks_of_month@ CHECK accepts: a non-empty subset of 1 to 5.
--
-- All 31 of them, not just the seven the schedule editor emits, because the column
-- holds any of the 31 and every reader has to answer for what it holds.
genStoredWeeks :: Gen [Int64]
genStoredWeeks = do
  weeks <- Gen.subsequence [1, 2, 3, 4, 5]
  case weeks of
    [] -> pure <$> Gen.element [1, 2, 3, 4, 5]
    _ -> pure weeks

-- | A week set as it might arrive from a form: shuffled, and possibly with repeats.
genUnnormalizedWeeks :: Gen [Int64]
genUnnormalizedWeeks = do
  weeks <- genStoredWeeks
  extra <- Gen.list (Range.linear 0 3) (Gen.element weeks)
  Gen.shuffle (weeks <> extra)

genRecurrence :: Gen Recurrence
genRecurrence = recurrenceFromRow <$> genDay <*> genStoredWeeks

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Domain.Types.Recurrence" $ do
  describe "round trip" $ do
    -- The path a stored schedule actually takes: the row is read into a Recurrence,
    -- the editor renders it, the form posts the numbers back, and the handler writes
    -- them again. Any step that loses information changes a show's schedule because
    -- somebody opened a page.
    it "survives the database column" $ hedgehog $ do
      recurrence <- forAll genRecurrence
      recurrenceFromRow (recurrenceDay recurrence) (weekNumbers recurrence) === recurrence

    it "survives the form post" $ hedgehog $ do
      recurrence <- forAll genRecurrence
      let dayText = dayOfWeekToPostgres (recurrenceDay recurrence)
      parseRecurrence dayText (weekNumbers recurrence) === Right recurrence

    it "survives a full column to form to column cycle" $ hedgehog $ do
      day <- forAll genDay
      weeks <- forAll genStoredWeeks
      let stored = recurrenceFromRow day weeks
          posted = parseRecurrence (dayOfWeekToPostgres day) (weekNumbers stored)
      fmap weekNumbers posted === Right (weekNumbers stored)

    it "reaches the same recurrence whichever constructor built it" $ hedgehog $ do
      day <- forAll genDay
      weeks <- forAll genStoredWeeks
      let viaRow = recurrenceFromRow day weeks
      viaRow === recurring day (recurrenceWeeks viaRow)

  describe "normalization" $ do
    it "ignores the order the weeks arrive in" $ hedgehog $ do
      day <- forAll genDay
      weeks <- forAll genUnnormalizedWeeks
      recurrenceFromRow day weeks === recurrenceFromRow day (sort weeks)

    it "ignores repeated weeks" $ hedgehog $ do
      day <- forAll genDay
      weeks <- forAll genStoredWeeks
      recurrenceFromRow day (weeks <> weeks) === recurrenceFromRow day weeks

    it "always yields sorted, duplicate-free weeks" $ hedgehog $ do
      recurrence <- forAll genRecurrence
      let ws = weekNumbers recurrence
      ws === sort ws
      length ws === length (NE.toList (NE.nub (recurrenceWeeks recurrence)))

    it "is idempotent" $ hedgehog $ do
      day <- forAll genDay
      weeks <- forAll genUnnormalizedWeeks
      let once = recurrenceFromRow day weeks
      recurrenceFromRow day (weekNumbers once) === once

  describe "frequencyLabel" $ do
    -- The catch-all in the old mapping had no case for all five weeks, so it called
    -- every weekly show twice-monthly. These two properties fail if that returns.
    it "says weekly exactly when the recurrence covers every week" $ hedgehog $ do
      recurrence <- forAll genRecurrence
      (frequencyLabel recurrence == "weekly") === airsEveryWeek recurrence

    it "says once exactly when the recurrence covers one week" $ hedgehog $ do
      recurrence <- forAll genRecurrence
      (frequencyLabel recurrence == "once") === (length (weekNumbers recurrence) == 1)

    it "only ever emits a key the editor understands" $ hedgehog $ do
      recurrence <- forAll genRecurrence
      assert (frequencyLabel recurrence `elem` (["weekly", "once", "twice"] :: [Text]))

    it "agrees with the editor on every week set the editor emits" $
      mapM_
        (\(weeks, expected) -> frequencyLabel (recurrenceFromRow Monday weeks) `shouldBe` expected)
        editorFrequencies

  describe "weeksLabel" $ do
    it "is absent exactly when the recurrence covers every week" $ hedgehog $ do
      recurrence <- forAll genRecurrence
      (weeksLabel recurrence == Nothing) === airsEveryWeek recurrence

    it "names one ordinal per week" $ hedgehog $ do
      recurrence <- forAll genRecurrence
      case weeksLabel recurrence of
        Nothing -> length (weekNumbers recurrence) === 5
        Just label -> length (splitOnAmp label) === length (weekNumbers recurrence)

    it "reads as expected for the sets the editor emits" $ do
      weeksLabel (recurrenceFromRow Monday [1, 2, 3, 4, 5]) `shouldBe` Nothing
      weeksLabel (recurrenceFromRow Monday [1, 3]) `shouldBe` Just "1st & 3rd"
      weeksLabel (recurrenceFromRow Monday [2, 4]) `shouldBe` Just "2nd & 4th"
      weeksLabel (recurrenceFromRow Monday [1]) `shouldBe` Just "1st"

  describe "formatRecurrence" $ do
    it "always names the weekday" $ hedgehog $ do
      recurrence <- forAll genRecurrence
      let phrase = formatRecurrence recurrence
      assert (dayOfWeekToPostgres (recurrenceDay recurrence) `isInfixOfLower` phrase)

    it "reads as a phrase" $ do
      formatRecurrence (recurrenceFromRow Monday [1, 2, 3, 4, 5]) `shouldBe` "Mondays"
      formatRecurrence (recurrenceFromRow Monday [1, 3]) `shouldBe` "1st & 3rd Mondays"
      formatRecurrence (recurrenceFromRow Sunday [2]) `shouldBe` "2nd Sundays"

  describe "parseWeeks" $ do
    it "accepts every week set the column can hold" $ hedgehog $ do
      weeks <- forAll genStoredWeeks
      fmap (map weekNumber . NE.toList) (parseWeeks weeks) === Right (sort weeks)

    it "rejects the empty list" $
      parseWeeks [] `shouldBe` Left "Pick at least one week of the month."

    it "rejects anything outside 1 to 5" $ hedgehog $ do
      bad <- forAll (Gen.filter (\w -> w < 1 || w > 5) (Gen.integral (Range.linear (-10) 20)))
      weeks <- forAll genStoredWeeks
      assert (isLeft (parseWeeks (bad : weeks)))

    it "names every out-of-range week, in the order it received them" $
      parseWeeks [1, 7, 0] `shouldBe` Left "Invalid week of month: 7, 0 (must be 1 to 5)."

  describe "mkWeekOfMonth" $ do
    it "accepts 1 to 5 and nothing else" $ hedgehog $ do
      n <- forAll (Gen.integral (Range.linear (-10) 20))
      (mkWeekOfMonth n /= Nothing) === (n >= 1 && n <= 5)

    it "round-trips through weekNumber" $ hedgehog $ do
      n <- forAll (Gen.integral (Range.linear 1 5))
      fmap weekNumber (mkWeekOfMonth n) === Just n

  describe "editorCanShow" $ do
    it "accepts exactly the sets the editor emits" $ hedgehog $ do
      recurrence <- forAll genRecurrence
      editorCanShow recurrence === (weekNumbers recurrence `elem` editorWeekSets)

    it "covers every set this spec names a frequency for" $
      map fst editorFrequencies `shouldMatchList` editorWeekSets

    it "rejects sets the buttons cannot reach" $
      mapM_
        (\weeks -> editorCanShow (recurrenceFromRow Monday weeks) `shouldBe` False)
        [[1, 2, 3], [1, 4], [5], [2, 3, 4, 5], [1, 2]]

  describe "everyWeek" $
    it "is the five-week set the handler writes for a weekly show" $ do
      map weekNumber (NE.toList everyWeek) `shouldBe` [1, 2, 3, 4, 5]
      airsEveryWeek (recurring Monday everyWeek) `shouldBe` True

--------------------------------------------------------------------------------

-- | The frequency button each of 'editorWeekSets' lights up.
editorFrequencies :: [([Int64], Text)]
editorFrequencies =
  [ ([1, 2, 3, 4, 5], "weekly"),
    ([1, 3], "twice"),
    ([2, 4], "twice"),
    ([1], "once"),
    ([2], "once"),
    ([3], "once"),
    ([4], "once")
  ]

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

splitOnAmp :: Text -> [Text]
splitOnAmp = Text.splitOn " & "

isInfixOfLower :: Text -> Text -> Bool
isInfixOfLower needle haystack = Text.toLower needle `Text.isInfixOf` Text.toLower haystack
