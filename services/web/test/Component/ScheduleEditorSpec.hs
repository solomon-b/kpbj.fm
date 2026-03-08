{-# LANGUAGE OverloadedRecordDot #-}
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
import Domain.Types.Timezone (addMinutesToTimeOfDay)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rel8 qualified
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
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
    it "returns [] for empty list" $ do
      schedulesToEditorJson [] `shouldBe` "[]"

    it "maps Nothing weeks to frequency 'weekly'" $ do
      let templates = [mkTemplate (Just Friday) Nothing (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
          result = decodeEditorJson (schedulesToEditorJson templates)
      case result of
        Just [slot] -> do
          lookupText "frequency" slot `shouldBe` Just "weekly"
          slot `shouldSatisfy` \s -> lookupMaybe "weeks" s == Just Aeson.Null
        _ -> fail "Expected single slot"

    it "maps [1,3] weeks to frequency 'twice'" $ do
      let templates = [mkTemplate (Just Monday) (Just [1, 3]) (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)]
          result = decodeEditorJson (schedulesToEditorJson templates)
      case result of
        Just [slot] -> lookupText "frequency" slot `shouldBe` Just "twice"
        _ -> fail "Expected single slot"

    it "maps [2,4] weeks to frequency 'twice'" $ do
      let templates = [mkTemplate (Just Monday) (Just [2, 4]) (TimeOfDay 8 0 0) (TimeOfDay 10 0 0)]
          result = decodeEditorJson (schedulesToEditorJson templates)
      case result of
        Just [slot] -> lookupText "frequency" slot `shouldBe` Just "twice"
        _ -> fail "Expected single slot"

    it "maps single-element weeks to frequency 'once'" $ do
      let templates = [mkTemplate (Just Wednesday) (Just [2]) (TimeOfDay 14 0 0) (TimeOfDay 15 0 0)]
          result = decodeEditorJson (schedulesToEditorJson templates)
      case result of
        Just [slot] -> lookupText "frequency" slot `shouldBe` Just "once"
        _ -> fail "Expected single slot"

    it "computes correct duration for 2-hour show" $ do
      let templates = [mkTemplate (Just Friday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 19 0 0) (TimeOfDay 21 0 0)]
          result = decodeEditorJson (schedulesToEditorJson templates)
      case result of
        Just [slot] -> lookupDuration slot `shouldBe` Just 120
        _ -> fail "Expected single slot"

    it "computes correct duration for 30-min show" $ do
      let templates = [mkTemplate (Just Saturday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 10 0 0) (TimeOfDay 10 30 0)]
          result = decodeEditorJson (schedulesToEditorJson templates)
      case result of
        Just [slot] -> lookupDuration slot `shouldBe` Just 30
        _ -> fail "Expected single slot"

    it "handles overnight show (end < start)" $ do
      let templates = [mkTemplate (Just Saturday) (Just [1, 2, 3, 4, 5]) (TimeOfDay 23 0 0) (TimeOfDay 1 0 0)]
          result = decodeEditorJson (schedulesToEditorJson templates)
      case result of
        Just [slot] -> lookupDuration slot `shouldBe` Just 120
        _ -> fail "Expected single slot"

    it "includes day in slot" $ do
      let templates = [mkTemplate (Just Tuesday) (Just [1]) (TimeOfDay 8 0 0) (TimeOfDay 9 0 0)]
          result = decodeEditorJson (schedulesToEditorJson templates)
      case result of
        Just [slot] -> lookupSlotDay slot `shouldBe` Just "tuesday"
        _ -> fail "Expected single slot"

    it "includes time in HH:MM format" $ do
      let templates = [mkTemplate (Just Tuesday) (Just [1]) (TimeOfDay 8 0 0) (TimeOfDay 9 0 0)]
          result = decodeEditorJson (schedulesToEditorJson templates)
      case result of
        Just [slot] -> lookupSlotTime slot `shouldBe` Just "08:00"
        _ -> fail "Expected single slot"

    it "handles multiple templates" $ do
      let templates =
            [ mkTemplate (Just Friday) Nothing (TimeOfDay 19 0 0) (TimeOfDay 21 0 0),
              mkTemplate (Just Monday) Nothing (TimeOfDay 8 0 0) (TimeOfDay 9 0 0)
            ]
          result = decodeEditorJson (schedulesToEditorJson templates)
      case result of
        Just slots -> length slots `shouldBe` 2
        _ -> fail "Expected two slots"

--------------------------------------------------------------------------------
-- Test Helpers

mkTemplate :: Maybe DayOfWeek -> Maybe [Int64] -> TimeOfDay -> TimeOfDay -> ShowSchedule.ScheduleTemplate Rel8.Result
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
      stAirsTwiceDaily = False
    }

-- | Decode the editor JSON into a list of Aeson objects for inspection.
decodeEditorJson :: Text -> Maybe [Aeson.Object]
decodeEditorJson = Aeson.decodeStrict . Text.encodeUtf8

-- | Look up a text value in a JSON object.
lookupText :: Aeson.Key -> Aeson.Object -> Maybe Text
lookupText key obj = case KeyMap.lookup key obj of
  Just (Aeson.String t) -> Just t
  _ -> Nothing

-- | Look up a possibly-null value.
lookupMaybe :: Aeson.Key -> Aeson.Object -> Maybe Aeson.Value
lookupMaybe = KeyMap.lookup

-- | Look up the duration from the first slot in a top-level editor object.
lookupDuration :: Aeson.Object -> Maybe Int
lookupDuration obj = case KeyMap.lookup "slots" obj of
  Just (Aeson.Array arr) -> case foldr (:) [] arr of
    (Aeson.Object slotObj : _) -> case KeyMap.lookup "duration" slotObj of
      Just (Aeson.Number n) -> Just (round n)
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

-- | Look up the day from the first slot.
lookupSlotDay :: Aeson.Object -> Maybe Text
lookupSlotDay obj = case KeyMap.lookup "slots" obj of
  Just (Aeson.Array arr) -> case foldr (:) [] arr of
    (Aeson.Object slotObj : _) -> lookupText "day" slotObj
    _ -> Nothing
  _ -> Nothing

-- | Look up the time from the first slot.
lookupSlotTime :: Aeson.Object -> Maybe Text
lookupSlotTime obj = case KeyMap.lookup "slots" obj of
  Just (Aeson.Array arr) -> case foldr (:) [] arr of
    (Aeson.Object slotObj : _) -> lookupText "time" slotObj
    _ -> Nothing
  _ -> Nothing
