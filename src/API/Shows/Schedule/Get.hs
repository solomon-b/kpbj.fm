{-# LANGUAGE ViewPatterns #-}

module API.Shows.Schedule.Get where

--------------------------------------------------------------------------------

import API.Shows.Schedule.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Coerce (coerce)
import Data.Either (isLeft, rights)
import Data.Has (Has)
import Data.Maybe (fromMaybe)
import Data.Time (addDays, getCurrentTime, utctDay)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import OrphanInstances.DayOfWeek (fromDayOfWeek, toDayOfWeek)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows/schedule"
    ( "shows"
        :> "schedule"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

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
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer (coerce -> cookie) (fromMaybe IsNotHxRequest -> htmxRequest) = do
  getUserInfo cookie >>= \(fmap snd -> mUserInfo) -> do
    -- Get current day of week and calculate week start (Monday)
    now <- liftIO getCurrentTime
    let today = utctDay now
        (_, _, currentDayOfWeek) = toDayOfWeek <$> toWeekDate today
        -- Calculate days to subtract to get to Monday (start of week)
        daysFromMonday = fromDayOfWeek currentDayOfWeek
        weekStart = addDays (negate daysFromMonday) today

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
        renderTemplate htmxRequest mUserInfo (template [] currentDayOfWeek (Just "Failed to load schedule. Please try again."))
      else
        renderTemplate htmxRequest mUserInfo (template scheduledShows currentDayOfWeek Nothing)
