module API.Schedule.Get.Templates.Page (template) where

--------------------------------------------------------------------------------

import API.Schedule.Get.Templates.Components (renderScheduleView)
import Data.Text (Text)
import Data.Time (Day, DayOfWeek, TimeOfDay)
import Design (base, tablet)
import Design.StyleBuilder (class_)
import Design.Tokens qualified as Tokens
import Domain.Types.WeekOffset (WeekOffset)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified

--------------------------------------------------------------------------------

-- | Schedule page template
template :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> WeekOffset -> Day -> Maybe Text -> Lucid.Html ()
template scheduledShows currentDayOfWeek currentTimeOfDay weekOffset weekStart maybeError = do
  -- Header
  Lucid.section_ [headerStyles] $ do
    Lucid.h1_ [class_ $ base [Tokens.heading2xl]] "SHOW SCHEDULE"
    Lucid.p_ [subtitleStyles] "Weekly broadcast schedule for KPBJ 95.9FM"

  -- Error message if present
  case maybeError of
    Just errorMsg -> Lucid.div_ [errorStyles] $ do
      Lucid.p_ [errorTextStyles] $ Lucid.toHtml errorMsg
    Nothing -> do
      -- Schedule View
      Lucid.section_ [sectionStyles] $ do
        renderScheduleView scheduledShows currentDayOfWeek currentTimeOfDay weekOffset weekStart

--------------------------------------------------------------------------------
-- Styles

headerStyles :: Lucid.Attributes
headerStyles = class_ $ do
  base [Tokens.bgWhite, Tokens.cardBorder, Tokens.fullWidth, "text-center"]
  base [Tokens.p6, Tokens.mb6]
  tablet [Tokens.p8, Tokens.mb8]

subtitleStyles :: Lucid.Attributes
subtitleStyles = class_ $ do
  base [Tokens.textGray600, Tokens.mb4]
  base [Tokens.textBase]
  tablet [Tokens.textLg, Tokens.mb6]

errorStyles :: Lucid.Attributes
errorStyles = class_ $ do
  base ["bg-red-100", Tokens.border2, "border-red-800", "text-center"]
  base [Tokens.p4, Tokens.mb6]
  tablet [Tokens.p6, Tokens.mb8]

errorTextStyles :: Lucid.Attributes
errorTextStyles = class_ $ do
  base ["text-red-800", Tokens.fontBold]

sectionStyles :: Lucid.Attributes
sectionStyles = class_ $ do
  base [Tokens.fullWidth]
  base [Tokens.mb6]
  tablet [Tokens.mb8]
