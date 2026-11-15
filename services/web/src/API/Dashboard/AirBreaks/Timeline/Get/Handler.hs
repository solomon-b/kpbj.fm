{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.AirBreaks.Timeline.Get.Handler
  ( handler,
    action,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Timeline.Get.Templates.Page (DayKind (..), NavPlacement (..), TimelineView (..), dayNav, template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Domain.DayTimeline (problems, scheduled, segmentViews)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Timezone (pacificDay, parseDateYMD)
import Effects.Clock (currentSystemTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Timeline (dayTimeline)
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/air-breaks/timeline.
handler ::
  Maybe Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler mDay cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Day timeline" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to view the timeline." userMetadata
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuery Shows.getAllActiveShows
        else execQuery (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows
    now <- lift currentSystemTime
    let day = fromMaybe (pacificDay now) (mDay >>= parseDateYMD)
    view <- action now day
    -- An HTMX request swaps only the main content, so it also sends the day
    -- navigation out of band to update the top bar.
    let content = case hxRequest of
          IsHxRequest -> template view <> dayNav OutOfBand view
          IsNotHxRequest -> template view
    lift $
      renderDashboardTemplate
        hxRequest
        userMetadata
        allShows
        selectedShow
        NavTimeline
        (Just (dayNav InTopBar view))
        Nothing
        content

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
