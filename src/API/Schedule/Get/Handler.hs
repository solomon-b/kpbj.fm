{-# LANGUAGE ViewPatterns #-}

module API.Schedule.Get.Handler where

--------------------------------------------------------------------------------

import API.Schedule.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Coerce (coerce)
import Data.Either (isLeft, rights)
import Data.Has (getter)
import Data.Maybe (fromMaybe)
import Data.Time (addDays, getCurrentTime, utcToLocalTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime (LocalTime (..), hoursToTimeZone)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.WeekOffset (WeekOffset (..))
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import OrphanInstances.DayOfWeek (toDayOfWeek)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Maybe WeekOffset ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer (fromMaybe 0 -> WeekOffset weekOffset) (coerce -> cookie) (fromMaybe IsNotHxRequest -> htmxRequest) = do
  storageBackend <- asks getter
  getUserInfo cookie >>= \(fmap snd -> mUserInfo) -> do
    -- Get current day of week and time in Pacific time
    nowUtc <- liftIO getCurrentTime
    let pacificTz = hoursToTimeZone (-8) -- PST is UTC-8
        nowPacific = utcToLocalTime pacificTz nowUtc
        today = localDay nowPacific
        currentTime = localTimeOfDay nowPacific
        (_, _, todayDayOfWeek) = toDayOfWeek <$> toWeekDate today
        -- Start from today and show next 7 days (rolling week view)
        weekStart = addDays (fromIntegral weekOffset * 7) today
        -- Only pass current day/time if viewing current week (offset = 0)
        currentDayOfWeek = if weekOffset == 0 then Just todayDayOfWeek else Nothing
        currentTimeOfDay = if weekOffset == 0 then Just currentTime else Nothing

        -- Generate all 7 days starting from today
        weekDays = [addDays i weekStart | i <- [0 .. 6]]

    -- Fetch schedule for each day of the week
    scheduleResults <- mapM (execQuerySpan . ShowSchedule.getScheduledShowsForDate) weekDays

    -- Flatten schedule results
    let scheduledShows = concat (rights scheduleResults)
        hasScheduleError = any isLeft scheduleResults

    if hasScheduleError
      then do
        Log.logInfo_ "Failed to fetch schedule from database"
        renderTemplate htmxRequest mUserInfo (template storageBackend [] currentDayOfWeek currentTimeOfDay (Just "Failed to load schedule. Please try again."))
      else
        renderTemplate htmxRequest mUserInfo (template storageBackend scheduledShows currentDayOfWeek currentTimeOfDay Nothing)
