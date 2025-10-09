{-# LANGUAGE OverloadedRecordDot #-}

module API.Episodes.Upload.Get.Templates.Fields
  ( renderShowOption,
    renderUpcomingDateOption,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, DayOfWeek (..), UTCTime)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified

--------------------------------------------------------------------------------

-- | Render a show option for the dropdown
renderShowOption :: Shows.Model -> Bool -> Lucid.Html ()
renderShowOption s isSelected = do
  Lucid.option_
    [ Lucid.value_ (Text.pack $ Prelude.show s.id),
      if isSelected then Lucid.selected_ "selected" else mempty
    ]
    $ Lucid.toHtml s.title

-- | Render an upcoming date option for the dropdown
renderUpcomingDateOption :: ShowSchedule.UpcomingShowDate -> Lucid.Html ()
renderUpcomingDateOption (ShowSchedule.UpcomingShowDate {usdShowDate = showDate, usdDayOfWeek = dow, usdStartTime = startTime, usdEndTime = endTime}) = do
  Lucid.option_
    [Lucid.value_ (Text.pack $ Prelude.show showDate)]
    $ Lucid.toHtml
    $ formatUpcomingDate dow showDate startTime endTime
  where
    formatUpcomingDate :: DayOfWeek -> Day -> UTCTime -> UTCTime -> Text
    formatUpcomingDate d sd st et =
      dayOfWeekName d
        <> ", "
        <> Text.pack (Prelude.show sd)
        <> " ("
        <> formatTime st
        <> " - "
        <> formatTime et
        <> ")"

    dayOfWeekName :: DayOfWeek -> Text
    dayOfWeekName Sunday = "Sunday"
    dayOfWeekName Monday = "Monday"
    dayOfWeekName Tuesday = "Tuesday"
    dayOfWeekName Wednesday = "Wednesday"
    dayOfWeekName Thursday = "Thursday"
    dayOfWeekName Friday = "Friday"
    dayOfWeekName Saturday = "Saturday"

    formatTime :: UTCTime -> Text
    formatTime = Text.pack . Prelude.show -- TODO: Format nicely
