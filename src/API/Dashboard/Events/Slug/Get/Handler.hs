{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.Slug.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Get.Templates.Page (template)
import API.Links (dashboardEventsLinks, rootLink)
import API.Types (DashboardEventsRoutes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Data.Either (fromRight)
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as Txn
import Lucid qualified
import Lucid.HTMX

--------------------------------------------------------------------------------

handler ::
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler eventId _slug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Event detail" (dashboardEventsLinks.list Nothing) $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuery Shows.getAllActiveShows
        else execQuery (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    -- 3. Fetch event with author
    (event, mAuthor) <- fetchEventData eventId

    -- 4. Render template
    let eventTemplate = template event mAuthor
    renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavEvents Nothing (Just actionButton) eventTemplate

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

fetchEventData ::
  Events.Id ->
  AppM (Events.Model, Maybe UserMetadata.Model)
fetchEventData eventId = do
  eventResult <- execTransaction $ do
    Txn.statement () (Events.getEventById eventId) >>= \case
      Nothing -> pure Nothing
      Just event -> do
        mAuthor <- Txn.statement () (UserMetadata.getUserMetadata event.emAuthorId)
        pure $ Just (event, mAuthor)

  case eventResult of
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Event"
    Right (Just result) -> pure result
