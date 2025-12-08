{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.DayOfWeek where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (DayOfWeek (..))
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..))
import Rel8 (DBEq, DBOrd, DBType (..), parseTypeInformation)

--------------------------------------------------------------------------------

-- | Convert DayOfWeek to PostgreSQL text representation
dayOfWeekToText :: DayOfWeek -> Text
dayOfWeekToText = \case
  Sunday -> "sunday"
  Monday -> "monday"
  Tuesday -> "tuesday"
  Wednesday -> "wednesday"
  Thursday -> "thursday"
  Friday -> "friday"
  Saturday -> "saturday"

-- | Parse DayOfWeek from PostgreSQL text representation
dayOfWeekFromText :: Text -> Maybe DayOfWeek
dayOfWeekFromText txt = case Text.toLower txt of
  "sunday" -> Just Sunday
  "monday" -> Just Monday
  "tuesday" -> Just Tuesday
  "wednesday" -> Just Wednesday
  "thursday" -> Just Thursday
  "friday" -> Just Friday
  "saturday" -> Just Saturday
  _ -> Nothing

instance EncodeValue DayOfWeek where
  encodeValue = Encoders.enum dayOfWeekToText

instance DecodeValue DayOfWeek where
  decodeValue = Decoders.enum dayOfWeekFromText

-- | DBType instance for DayOfWeek for rel8 queries.
instance DBType DayOfWeek where
  typeInformation =
    parseTypeInformation
      ( \case
          "sunday" -> Right Sunday
          "monday" -> Right Monday
          "tuesday" -> Right Tuesday
          "wednesday" -> Right Wednesday
          "thursday" -> Right Thursday
          "friday" -> Right Friday
          "saturday" -> Right Saturday
          other -> Left $ "Invalid DayOfWeek: " <> Text.unpack other
      )
      dayOfWeekToText
      typeInformation

-- | DBEq instance for DayOfWeek for rel8 queries.
instance DBEq DayOfWeek

-- | DBOrd instance for DayOfWeek for rel8 queries.
instance DBOrd DayOfWeek

--------------------------------------------------------------------------------

-- | Convert ISO week date day number (1-7) to DayOfWeek (Monday-Sunday)
toDayOfWeek :: Int -> DayOfWeek
toDayOfWeek = \case
  1 -> Monday
  2 -> Tuesday
  3 -> Wednesday
  4 -> Thursday
  5 -> Friday
  6 -> Saturday
  7 -> Sunday
  _ -> Monday

fromDayOfWeek :: DayOfWeek -> Integer
fromDayOfWeek = \case
  Monday -> 0
  Tuesday -> 1
  Wednesday -> 2
  Thursday -> 3
  Friday -> 4
  Saturday -> 5
  Sunday -> 6
