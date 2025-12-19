{-# LANGUAGE QuasiQuotes #-}

module API.Schedule.Get.Templates.Page (template) where

--------------------------------------------------------------------------------

import API.Schedule.Get.Templates.Components (renderScheduleView)
import Data.Text (Text)
import Data.Time (Day, DayOfWeek, TimeOfDay)
import Design.StyleBuilder (base, styles, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.WeekOffset (WeekOffset)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified

--------------------------------------------------------------------------------

-- | Schedule page template
template :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> WeekOffset -> Day -> Maybe Text -> Lucid.Html ()
template scheduledShows currentDayOfWeek currentTimeOfDay weekOffset weekStart maybeError = do
  -- Header
  Lucid.section_ [Lucid.class_ headerStyles] $ do
    Lucid.h1_ [Lucid.class_ Tokens.heading2xl] "SHOW SCHEDULE"
    Lucid.p_ [Lucid.class_ subtitleStyles] "Weekly broadcast schedule for KPBJ 95.9FM"

  -- Error message if present
  case maybeError of
    Just errorMsg -> Lucid.div_ [Lucid.class_ errorStyles] $ do
      Lucid.p_ [Lucid.class_ errorTextStyles] $ Lucid.toHtml errorMsg
    Nothing -> do
      -- Schedule View
      Lucid.section_ [Lucid.class_ sectionStyles] $ do
        renderScheduleView scheduledShows currentDayOfWeek currentTimeOfDay weekOffset weekStart

--------------------------------------------------------------------------------
-- Styles

headerStyles :: Text
headerStyles = styles $ do
  base [Tokens.bgWhite, Tokens.cardBorder, Tokens.fullWidth, "text-center"]
  base [Tokens.p6, Tokens.mb6]
  tablet [Tokens.p8, Tokens.mb8]

subtitleStyles :: Text
subtitleStyles = styles $ do
  base [Tokens.textGray600, Tokens.mb4]
  base [Tokens.textBase]
  tablet [Tokens.textLg, Tokens.mb6]

errorStyles :: Text
errorStyles = styles $ do
  base ["bg-red-100", Tokens.border2, "border-red-800", "text-center"]
  base [Tokens.p4, Tokens.mb6]
  tablet [Tokens.p6, Tokens.mb8]

errorTextStyles :: Text
errorTextStyles = styles $ do
  base ["text-red-800", Tokens.fontBold]

sectionStyles :: Text
sectionStyles = styles $ do
  base [Tokens.fullWidth]
  base [Tokens.mb6]
  tablet [Tokens.mb8]
