{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.AirBreaks.Timeline.Get.Handler
  ( handler,
    action,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Timeline.Get.Templates.Page (DayKind (..), TimelineView (..), dayNav, template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Auth.Roles (RoleAuth (..), Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Domain.DayTimeline (problems, scheduled, segmentViews)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Timezone (pacificDay, parseDateYMD)
import Effects.Clock (currentSystemTime)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Effects.Timeline (dayTimeline)
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/air-breaks/timeline.
handler ::
  Maybe Text ->
  Satisfies 'Staff RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler mDay (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Day timeline" apiLinks.rootGet $ do
    now <- lift currentSystemTime
    let day = fromMaybe (pacificDay now) (mDay >>= parseDateYMD)
    view <- action now day
    lift $
      renderDashboardTemplate
        hxRequest
        auth.raMetadata
        NoShow
        NavTimeline
        (Just (dayNav view))
        Nothing
        (template view)

-- | The timeline of a day.
--
-- Viewing today builds today's break plan if the first @/now@ request of the
-- day has not. A future day uses the forecast, which is not stored.
action ::
  -- | The current time
  UTCTime ->
  -- | The day to show
  Day ->
  ExceptT HandlerError AppM TimelineView
action now day = do
  timeline <- fromRightM throwDatabaseError $ lift $ dayTimeline now day
  let views = scheduled (segmentViews timeline)
  pure
    TimelineView
      { tvDay = day,
        tvKind = case compare day (pacificDay now) of
          LT -> PastDay
          EQ -> Today
          GT -> FutureDay,
        tvNow = now,
        tvSegments = views,
        tvProblems = problems views
      }
