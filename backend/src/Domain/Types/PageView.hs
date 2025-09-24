module Domain.Types.PageView where

--------------------------------------------------------------------------------

import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (MonthOfYear, Year)
import Servant qualified

--------------------------------------------------------------------------------

data PageView = ListView | MonthView Year MonthOfYear | WeekView Year Int
  deriving (Show, Eq)

isMonthView :: PageView -> Bool
isMonthView = \case
  MonthView _ _ -> True
  _ -> False

isWeekView :: PageView -> Bool
isWeekView = \case
  WeekView _ _ -> True
  _ -> False

instance Servant.FromHttpApiData PageView where
  parseQueryParam pageView =
    case Text.splitOn "-" pageView of
      ["month", yearText, monthText] ->
        let (year, month) = (read @Year $ Text.unpack yearText, read @MonthOfYear $ Text.unpack monthText)
         in Right $ MonthView year month
      ["week", yearText, weekText] ->
        let (year, weekNum) = (read @Year $ Text.unpack yearText, read @Int $ Text.unpack weekText)
         in Right $ WeekView year weekNum
      ["list"] -> Right ListView
      _ -> Left "Invalid view query param value."

instance Servant.ToHttpApiData PageView where
  toQueryParam = \case
    ListView -> "list"
    MonthView year month -> "month-" <> display year <> "-" <> display month
    WeekView year weekNum -> "week-" <> display year <> "-" <> display weekNum
