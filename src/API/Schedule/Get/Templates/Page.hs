module API.Schedule.Get.Templates.Page (template) where

--------------------------------------------------------------------------------

import API.Schedule.Get.Templates.Components (renderScheduleView)
import Data.Text (Text)
import Data.Time (Day, DayOfWeek, TimeOfDay)
import Design (base, desktop, tablet)
import Design.StyleBuilder (class_)
import Design.Tokens qualified as Tokens
import Domain.Types.WeekOffset (WeekOffset)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified

--------------------------------------------------------------------------------

-- | Schedule page template
template :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> WeekOffset -> Day -> Maybe Text -> Lucid.Html ()
template scheduledShows currentDayOfWeek currentTimeOfDay weekOffset weekStart maybeError = do
  -- Header (desktop only - mobile has its own in the schedule component)
  Lucid.h1_ [desktopHeaderStyles] "Schedule"

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

desktopHeaderStyles :: Lucid.Attributes
desktopHeaderStyles = class_ $ do
  base ["hidden"]
  desktop [Tokens.fullWidth, Tokens.text2xl, Tokens.fontBold, Tokens.mb8, "block"]
