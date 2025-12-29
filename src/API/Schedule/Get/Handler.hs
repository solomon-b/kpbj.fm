{-# LANGUAGE ViewPatterns #-}

module API.Schedule.Get.Handler where

--------------------------------------------------------------------------------

import API.Schedule.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Coerce (coerce)
import Data.Either (isLeft, rights)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.Time (addDays, getCurrentTime, utcToLocalTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime (LocalTime (..), hoursToTimeZone)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.WeekOffset (WeekOffset (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import OrphanInstances.DayOfWeek (fromDayOfWeek, toDayOfWeek)

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Maybe WeekOffset ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer (fromMaybe 0 -> WeekOffset weekOffset) (coerce -> cookie) (fromMaybe IsNotHxRequest -> htmxRequest) = do
  getUserInfo cookie >>= \(fmap snd -> mUserInfo) -> do
    -- Get current day of week and time in Pacific time
    nowUtc <- liftIO getCurrentTime
    let pacificTz = hoursToTimeZone (-8) -- PST is UTC-8
        nowPacific = utcToLocalTime pacificTz nowUtc
        today = localDay nowPacific
        currentTime = localTimeOfDay nowPacific
        (_, _, todayDayOfWeek) = toDayOfWeek <$> toWeekDate today
        -- Calculate days to subtract to get to Monday (start of week)
        daysFromMonday = fromDayOfWeek todayDayOfWeek
        currentWeekStart = addDays (negate daysFromMonday) today
        -- Apply week offset (7 days per week)
        weekStart = addDays (fromIntegral weekOffset * 7) currentWeekStart
        -- Only pass current day/time if viewing current week
        currentDayOfWeek = if weekOffset == 0 then Just todayDayOfWeek else Nothing
        currentTimeOfDay = if weekOffset == 0 then Just currentTime else Nothing

        -- Generate all 7 days of the week
        weekDays = [addDays i weekStart | i <- [0 .. 6]]

    -- Fetch schedule for each day of the week
    scheduleResults <- mapM (execQuerySpan . ShowSchedule.getScheduledShowsForDate) weekDays

    -- Flatten schedule results
    let scheduledShows = concat (rights scheduleResults)
        hasScheduleError = any isLeft scheduleResults

    if hasScheduleError
      then do
        Log.logInfo_ "Failed to fetch schedule from database"
        renderTemplate htmxRequest mUserInfo (template [] currentDayOfWeek currentTimeOfDay (Just "Failed to load schedule. Please try again."))
      else
        renderTemplate htmxRequest mUserInfo (template scheduledShows currentDayOfWeek currentTimeOfDay Nothing)
