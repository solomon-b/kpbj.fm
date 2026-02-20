{-# LANGUAGE ViewPatterns #-}

module API.Schedule.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Schedule.Get.Templates.Page (template)
import API.Types (Routes (..))
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Either (lefts, rights)
import Data.Functor ((<&>))
import Data.Has (getter)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day, DayOfWeek, addDays, getCurrentTime, utcToLocalTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay, hoursToTimeZone)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.StorageBackend (StorageBackend)
import Domain.Types.WeekOffset (WeekOffset (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Log qualified
import Lucid qualified
import OrphanInstances.DayOfWeek (toDayOfWeek)

--------------------------------------------------------------------------------

data ScheduleViewData = ScheduleViewData
  { svdStorageBackend :: StorageBackend,
    svdScheduledShows :: [ShowSchedule.ScheduledShowWithDetails],
    svdWeekDays :: [Day],
    svdCurrentDayOfWeek :: Maybe DayOfWeek,
    svdCurrentTimeOfDay :: Maybe TimeOfDay,
    svdMaybeError :: Maybe Text
  }

--------------------------------------------------------------------------------

handler ::
  Maybe WeekOffset ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler mWeekOffset (coerce -> cookie) (fromMaybe IsNotHxRequest -> htmxRequest) =
  handleHtmlErrors "Schedule" apiLinks.rootGet $ do
    mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
    vd <- action mWeekOffset
    lift $
      renderTemplate
        htmxRequest
        mUserInfo
        ( template
            vd.svdStorageBackend
            vd.svdScheduledShows
            vd.svdWeekDays
            vd.svdCurrentDayOfWeek
            vd.svdCurrentTimeOfDay
            vd.svdMaybeError
        )

--------------------------------------------------------------------------------

-- | Business logic: fetch schedule for a week, compute current day/time context.
action ::
  Maybe WeekOffset ->
  ExceptT HandlerError AppM ScheduleViewData
action (fromMaybe 0 -> WeekOffset weekOffset) = do
  storageBackend <- asks getter

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

  scheduleResults <- mapM (execQuery . ShowSchedule.getScheduledShowsForDate) weekDays

  let scheduledShows = concat (rights scheduleResults)
      errors = lefts scheduleResults

  forM_ errors $ \err ->
    Log.logInfo "Failed to fetch schedule from database" (Aeson.object ["error" .= show err])

  let maybeError =
        if not (null errors)
          then Just "Failed to load schedule. Please try again."
          else Nothing

  pure
    ScheduleViewData
      { svdStorageBackend = storageBackend,
        svdScheduledShows = scheduledShows,
        svdWeekDays = weekDays,
        svdCurrentDayOfWeek = currentDayOfWeek,
        svdCurrentTimeOfDay = currentTimeOfDay,
        svdMaybeError = maybeError
      }
