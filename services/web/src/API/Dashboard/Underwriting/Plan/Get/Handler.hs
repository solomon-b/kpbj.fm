{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Underwriting.Plan.Get.Handler
  ( handler,
    action,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Underwriting.Plan.Get.Templates.Page (PlanView (..), template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Timezone (pacificDay, parseDateYMD)
import Effects.BreakPlan qualified as BreakPlan
import Effects.Clock (currentSystemTime)
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/underwriting/plan.
handler ::
  Maybe Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler mDay cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Break plan" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to manage underwriting." userMetadata
    now <- lift currentSystemTime
    let day = fromMaybe (pacificDay now) (mDay >>= parseDateYMD)
    view <- action now day
    lift $
      renderDashboardTemplate
        hxRequest
        userMetadata
        []
        Nothing
        NavBreakPlan
        Nothing
        Nothing
        (template day view)

-- | The plan for a day.
--
-- Only today's plan is ever built. Viewing today builds it if the first break
-- request has not. A past day shows what was planned. A future day shows a
-- forecast, which is not stored.
action ::
  -- | The current time
  UTCTime ->
  -- | The day to show
  Day ->
  ExceptT HandlerError AppM PlanView
action now day
  | day > today =
      ForecastPlan <$> fromRightM throwDatabaseError (lift (BreakPlan.forecastDay now day))
  | otherwise = do
      when (day == today) $
        fromRightM throwDatabaseError $
          lift $
            BreakPlan.ensurePlan day
      tracks <- fromRightM throwDatabaseError $ lift $ BreakPlan.tracksForDay day
      pure $ if day == today then TodayPlan tracks else PastPlan tracks
  where
    today = pacificDay now
