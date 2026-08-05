{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the ScheduleEditor component's pure conversion functions
-- and the shared addMinutesToTimeOfDay utility.
module Component.ScheduleEditorSpec where

--------------------------------------------------------------------------------

import Component.ScheduleEditor (schedulesToEditorJson)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Time (DayOfWeek (..), TimeOfDay (..), UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Data.Vector qualified as Vector
import Domain.Types.Timezone (addMinutesToTimeOfDay)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rel8 qualified
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "addMinutesToTimeOfDay" $ do
    it "adds minutes within the same hour" $ do
      addMinutesToTimeOfDay (TimeOfDay 8 0 0) 30 `shouldBe` TimeOfDay 8 30 0

    it "adds minutes crossing hour boundary" $ do
      addMinutesToTimeOfDay (TimeOfDay 8 30 0) 60 `shouldBe` TimeOfDay 9 30 0

    it "adds zero minutes (identity)" $ do
      addMinutesToTimeOfDay (TimeOfDay 14 15 0) 0 `shouldBe` TimeOfDay 14 15 0

    it "wraps past midnight" $ do
      addMinutesToTimeOfDay (TimeOfDay 23 0 0) 120 `shouldBe` TimeOfDay 1 0 0

    it "wraps exactly to midnight" $ do
      addMinutesToTimeOfDay (TimeOfDay 22 0 0) 120 `shouldBe` TimeOfDay 0 0 0

    it "preserves seconds" $ do
      addMinutesToTimeOfDay (TimeOfDay 8 0 42) 60 `shouldBe` TimeOfDay 9 0 42

    it "prop: adding 0 is identity" $ hedgehog $ do
      h <- forAll $ Gen.int (Range.linear 0 23)
      m <- forAll $ Gen.int (Range.linear 0 59)
      let t = TimeOfDay h m 0
      addMinutesToTimeOfDay t 0 === t

    it "prop: seconds are preserved" $ hedgehog $ do
      h <- forAll $ Gen.int (Range.linear 0 23)
      m <- forAll $ Gen.int (Range.linear 0 59)
      s <- forAll $ Gen.realFrac_ (Range.linearFrac 0 59)
      mins <- forAll $ Gen.int (Range.linear 0 1440)
      let t = TimeOfDay h m s
          result = addMinutesToTimeOfDay t mins
      todSec result === s

    it "prop: result hour is always 0-23" $ hedgehog $ do
      h <- forAll $ Gen.int (Range.linear 0 23)
      m <- forAll $ Gen.int (Range.linear 0 59)
      mins <- forAll $ Gen.int (Range.linear 0 2880)
      let result = addMinutesToTimeOfDay (TimeOfDay h m 0) mins
      assert $ todHour result >= 0 && todHour result <= 23
      assert $ todMin result >= 0 && todMin result <= 59

  describe "schedulesToEditorJson" $ do
    it "returns null when the show holds no slot" $ do
      schedulesToEditorJson Nothing `shouldBe` "null"

    it "maps every week to frequency 'weekly'" $ do
      -- The old mapping had no case for [1,2,3,4,5] and fell through to "twice",
      -- so every weekly show opened the edit form reading TWICE A MONTH.
      frequencyOf (mkTemplate Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0))
        `shouldBe` Just "weekly"

    it "maps [1,3] weeks to frequency 'twice'" $ do
      frequencyOf (mkTemplate Monday [1, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0))
        `shouldBe` Just "twice"

    it "maps [2,4] weeks to frequency 'twice'" $ do
      frequencyOf (mkTemplate Monday [2, 4] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0))
        `shouldBe` Just "twice"

    it "maps single-element weeks to frequency 'once'" $ do
      frequencyOf (mkTemplate Wednesday [2] (TimeOfDay 14 0 0) (TimeOfDay 15 0 0))
        `shouldBe` Just "once"

    it "carries the weeks through unchanged" $ do
      fieldOf "weeks" (mkTemplate Monday [1, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0))
        `shouldBe` Just (Aeson.Array (Vector.fromList [Aeson.Number 1, Aeson.Number 3]))

    it "sorts and deduplicates stored weeks" $ do
      -- A stored [3,1,3] describes the same recurrence as [1,3]. The editor
      -- compares its week buttons with JSON.stringify, so the order matters.
      fieldOf "weeks" (mkTemplate Monday [3, 1, 3] (TimeOfDay 8 0 0) (TimeOfDay 10 0 0))
        `shouldBe` Just (Aeson.Array (Vector.fromList [Aeson.Number 1, Aeson.Number 3]))

    it "computes correct duration for 2-hour show" $ do
      durationOf (mkTemplate Friday [1, 2, 3, 4, 5] (TimeOfDay 19 0 0) (TimeOfDay 21 0 0))
        `shouldBe` Just 120

    it "computes correct duration for 30-min show" $ do
      durationOf (mkTemplate Saturday [1, 2, 3, 4, 5] (TimeOfDay 10 0 0) (TimeOfDay 10 30 0))
        `shouldBe` Just 30

    it "handles overnight show (end < start)" $ do
      durationOf (mkTemplate Saturday [1, 2, 3, 4, 5] (TimeOfDay 23 0 0) (TimeOfDay 1 0 0))
        `shouldBe` Just 120

    it "includes the day" $ do
      textOf "day" (mkTemplate Tuesday [1] (TimeOfDay 8 0 0) (TimeOfDay 9 0 0))
        `shouldBe` Just "tuesday"

    it "includes time in HH:MM format" $ do
      textOf "time" (mkTemplate Tuesday [1] (TimeOfDay 8 0 0) (TimeOfDay 9 0 0))
        `shouldBe` Just "08:00"

--------------------------------------------------------------------------------
-- Test Helpers

mkTemplate :: DayOfWeek -> [Int64] -> TimeOfDay -> TimeOfDay -> ShowSchedule.ScheduleTemplate Rel8.Result
mkTemplate dow weeks start end =
  ShowSchedule.ScheduleTemplate
    { stId = ShowSchedule.TemplateId 1,
      stShowId = Shows.Id 1,
      stDayOfWeek = dow,
      stWeeksOfMonth = weeks,
      stStartTime = start,
      stEndTime = end,
      stTimezone = "America/Los_Angeles",
      stCreatedAt = UTCTime (fromGregorian 2025 1 1) 0,
      stReplayStartTime = Nothing
    }

-- | Decode the editor JSON, which is one object or null.
decodeEditorJson :: Text -> Maybe Aeson.Object
decodeEditorJson = Aeson.decodeStrict . Text.encodeUtf8

-- | A field of the encoded template.
fieldOf :: Aeson.Key -> ShowSchedule.ScheduleTemplate Rel8.Result -> Maybe Aeson.Value
fieldOf key template = decodeEditorJson (schedulesToEditorJson (Just template)) >>= KeyMap.lookup key

textOf :: Aeson.Key -> ShowSchedule.ScheduleTemplate Rel8.Result -> Maybe Text
textOf key template = case fieldOf key template of
  Just (Aeson.String t) -> Just t
  _ -> Nothing

frequencyOf :: ShowSchedule.ScheduleTemplate Rel8.Result -> Maybe Text
frequencyOf = textOf "frequency"

durationOf :: ShowSchedule.ScheduleTemplate Rel8.Result -> Maybe Int
durationOf template = case fieldOf "duration" template of
  Just (Aeson.Number n) -> Just (round n)
  _ -> Nothing
