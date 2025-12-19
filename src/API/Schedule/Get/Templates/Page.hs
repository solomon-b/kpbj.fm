{-# LANGUAGE QuasiQuotes #-}

module API.Schedule.Get.Templates.Page (template) where

--------------------------------------------------------------------------------

import API.Schedule.Get.Templates.Components (renderScheduleView)
import Data.Text (Text)
import Data.Time (Day, DayOfWeek, TimeOfDay)
import Design.Tokens qualified as Tokens
import Domain.Types.WeekOffset (WeekOffset)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified
import Lucid.Responsive (cls)

--------------------------------------------------------------------------------

-- | Schedule page template
template :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> WeekOffset -> Day -> Maybe Text -> Lucid.Html ()
template scheduledShows currentDayOfWeek currentTimeOfDay weekOffset weekStart maybeError = do
  -- Header
  Lucid.section_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, Tokens.mb8, "text-center", Tokens.fullWidth]] $ do
    Lucid.h1_ [Lucid.class_ Tokens.heading2xl] "SHOW SCHEDULE"
    Lucid.p_ [Lucid.class_ $ cls [Tokens.textLg, Tokens.textGray600, Tokens.mb6]] "Weekly broadcast schedule for KPBJ 95.9FM"

  -- Error message if present
  case maybeError of
    Just errorMsg -> Lucid.div_ [Lucid.class_ $ cls ["bg-red-100", Tokens.border2, "border-red-800", Tokens.p6, Tokens.mb8, "text-center"]] $ do
      Lucid.p_ [Lucid.class_ $ cls ["text-red-800", Tokens.fontBold]] $ Lucid.toHtml errorMsg
    Nothing -> do
      -- Schedule View
      Lucid.section_ [Lucid.class_ $ cls [Tokens.mb8, Tokens.fullWidth]] $ do
        renderScheduleView scheduledShows currentDayOfWeek currentTimeOfDay weekOffset weekStart
