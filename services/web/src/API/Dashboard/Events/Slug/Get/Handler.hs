{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.Slug.Get.Handler (handler, action, EventDetailViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Get.Templates.Page (template)
import API.Links (dashboardEventsLinks, rootLink)
import API.Types (DashboardEventsRoutes (..))
import App.Auth.Roles (RoleAuth (..), Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execTransaction)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as Txn
import Lucid qualified
import Lucid.HTMX
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the event detail page.
data EventDetailViewData = EventDetailViewData
  { edvUserMetadata :: UserMetadata.Model,
    edvEvent :: Events.Model,
    edvAuthor :: Maybe UserMetadata.Model
  }

-- | Business logic: fetch the event with its author.
action ::
  UserMetadata.Model ->
  Events.Id ->
  ExceptT HandlerError AppM EventDetailViewData
action userMetadata eventId = do
  (event, mAuthor) <- fetchEventData eventId

  pure
    EventDetailViewData
      { edvUserMetadata = userMetadata,
        edvEvent = event,
        edvAuthor = mAuthor
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Events.Id ->
  Slug ->
  Satisfies 'UserMetadata.Staff RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler eventId _slug (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Event detail" (dashboardEventsLinks.list Nothing) $ do
    vd <- action auth.raMetadata eventId
    let eventTemplate = template vd.edvEvent vd.edvAuthor
    lift $
      renderDashboardTemplate
        hxRequest
        vd.edvUserMetadata
        NoShow
        NavEvents
        Nothing
        (Just actionButton)
        eventTemplate

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

fetchEventData ::
  Events.Id ->
  ExceptT HandlerError AppM (Events.Model, Maybe UserMetadata.Model)
fetchEventData eventId =
  fromMaybeM (throwNotFound "Event") $
    fromRightM throwDatabaseError $
      execTransaction $ do
        Txn.statement () (Events.getEventById eventId) >>= \case
          Nothing -> pure Nothing
          Just event -> do
            mAuthor <- Txn.statement () (UserMetadata.getUserMetadata event.emAuthorId)
            pure $ Just (event, mAuthor)
