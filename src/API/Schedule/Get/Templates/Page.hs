module API.Schedule.Get.Templates.Page (template) where

--------------------------------------------------------------------------------

import API.Schedule.Get.Templates.Components (renderScheduleView)
import Data.Text (Text)
import Data.Time (DayOfWeek, TimeOfDay)
import Design (base, tablet)
import Design.StyleBuilder (class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified

--------------------------------------------------------------------------------

-- | Schedule page template (single-day view with day navigation)
template :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> Maybe Text -> Lucid.Html ()
template scheduledShows currentDayOfWeek currentTimeOfDay maybeError = do
  -- Error message if present
  case maybeError of
    Just errorMsg -> Lucid.div_ [errorStyles] $ do
      Lucid.p_ [errorTextStyles] $ Lucid.toHtml errorMsg
    Nothing -> do
      -- Schedule View
      Lucid.section_ [sectionStyles] $ do
        renderScheduleView scheduledShows currentDayOfWeek currentTimeOfDay

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
