{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.Get.Handler (handler, action, EventListViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.Events.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardEventsLinks, rootLink)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the events list page.
data EventListViewData = EventListViewData
  { elvUserMetadata :: UserMetadata.Model,
    elvAllShows :: [Shows.Model],
    elvSelectedShow :: Maybe Shows.Model,
    elvEvents :: [Events.Model],
    elvPage :: Int64,
    elvHasMore :: Bool
  }

-- | Business logic: pagination setup, fetch shows and events.
action ::
  User.Model ->
  UserMetadata.Model ->
  Maybe Int64 ->
  ExceptT HandlerError AppM EventListViewData
action user userMetadata maybePage = do
  -- 1. Set up pagination
  let page = fromMaybe 1 maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset

  -- 2. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
      selectedShow = listToMaybe allShows

  -- 3. Fetch events
  allEvents <- fetchEvents limit offset

  -- 4. Build paginated result
  let events = take (fromIntegral limit) allEvents
      hasMore = length allEvents > fromIntegral limit

  pure
    EventListViewData
      { elvUserMetadata = userMetadata,
        elvAllShows = allShows,
        elvSelectedShow = selectedShow,
        elvEvents = events,
        elvPage = page,
        elvHasMore = hasMore
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe Int64 ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Events list" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata maybePage
    let isAppendRequest = hxRequest == IsHxRequest && vd.elvPage > 1
    if isAppendRequest
      then pure $ renderItemsFragment vd.elvEvents vd.elvPage vd.elvHasMore
      else do
        let eventsTemplate = template vd.elvEvents vd.elvPage vd.elvHasMore
        lift $
          renderDashboardTemplate
            hxRequest
            vd.elvUserMetadata
            vd.elvAllShows
            vd.elvSelectedShow
            NavEvents
            Nothing
            (Just actionButton)
            eventsTemplate

fetchEvents :: Limit -> Offset -> ExceptT HandlerError AppM [Events.Model]
fetchEvents limit offset =
  fromRightM throwDatabaseError $
    execQuery (Events.getAllEvents (limit + 1) offset)

-- | Action button for creating new event
actionButton :: Lucid.Html ()
actionButton =
  let newEventUrl = rootLink dashboardEventsLinks.newGet
   in Lucid.a_
        [ Lucid.href_ newEventUrl,
          hxGet_ newEventUrl,
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base [Tokens.bgAlt, Tokens.fgPrimary, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, Tokens.hoverBg]
        ]
        "New Event"
