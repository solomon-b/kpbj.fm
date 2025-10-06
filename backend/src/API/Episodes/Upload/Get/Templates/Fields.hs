{-# LANGUAGE OverloadedRecordDot #-}

module API.Episodes.Upload.Get.Templates.Fields
  ( renderShowOption,
    renderUpcomingDateOption,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (DayOfWeek (..), UTCTime)
import Effects.Database.Tables.Show qualified as Show
import Lucid qualified

--------------------------------------------------------------------------------

-- | Render a show option for the dropdown
renderShowOption :: Show.ShowModel -> Bool -> Lucid.Html ()
renderShowOption s isSelected = do
  Lucid.option_
    [ Lucid.value_ (Text.pack $ Prelude.show s.id),
      if isSelected then Lucid.selected_ "selected" else mempty
    ]
    $ Lucid.toHtml s.title

-- | Render an upcoming date option for the dropdown
renderUpcomingDateOption :: Show.UpcomingShowDate -> Lucid.Html ()
renderUpcomingDateOption date = do
  Lucid.option_
    [Lucid.value_ (Text.pack $ Prelude.show date.usdShowDate)]
    $ Lucid.toHtml
    $ formatUpcomingDate date
  where
    formatUpcomingDate :: Show.UpcomingShowDate -> Text
    formatUpcomingDate d =
      dayOfWeekName d.usdDayOfWeek
        <> ", "
        <> Text.pack (Prelude.show d.usdShowDate)
        <> " ("
        <> formatTime d.usdStartTime
        <> " - "
        <> formatTime d.usdEndTime
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
