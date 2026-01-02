{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.Events.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardEventsLinks, rootLink)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)

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
  Maybe Int64 ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Events list" apiLinks.rootGet $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Set up pagination
    let page = fromMaybe 1 maybePage
        limit = 20 :: Limit
        offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
        isAppendRequest = hxRequest == IsHxRequest && page > 1

    -- 3. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuerySpan Shows.getAllActiveShows
        else execQuerySpan (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    -- 4. Fetch events
    allEvents <- fetchEvents limit offset

    -- 5. Render response
    let events = take (fromIntegral limit) allEvents
        hasMore = length allEvents > fromIntegral limit

    if isAppendRequest
      then pure $ renderItemsFragment events page hasMore
      else do
        let eventsTemplate = template events page hasMore
        renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavEvents Nothing (Just actionButton) eventsTemplate

fetchEvents :: (MonadUnliftIO m, MonadDB m, Log.MonadLog m, MonadReader env m, Has Tracer env, MonadThrow m) => Limit -> Offset -> m [Events.Model]
fetchEvents limit offset = do
  eventsResult <- getEventsResults limit offset
  case eventsResult of
    Left err -> throwDatabaseError err
    Right events -> pure events

-- | Action button for creating new event
actionButton :: Lucid.Html ()
actionButton =
  let newEventUrl = rootLink dashboardEventsLinks.newGet
   in Lucid.a_
        [ Lucid.href_ newEventUrl,
          hxGet_ newEventUrl,
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
        ]
        "New Event"

getEventsResults ::
  ( MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env
  ) =>
  Limit ->
  Offset ->
  m (Either HSQL.Pool.UsageError [Events.Model])
getEventsResults limit offset =
  execQuerySpan (Events.getAllEvents (limit + 1) offset)
