{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Delete.Handler (handler, action) where

--------------------------------------------------------------------------------

import App.Auth.Roles (RoleAuth, Satisfies)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Business logic: fetch, delete.
action ::
  Events.Id ->
  ExceptT HandlerError AppM ()
action eventId = do
  event <- fetchEvent eventId
  deleteEvent event

-- | Servant handler: thin glue composing action + banner response.
handler ::
  Events.Id ->
  Slug ->
  Satisfies 'UserMetadata.Staff RoleAuth ->
  AppM (Lucid.Html ())
handler eventId _eventSlug _ =
  handleBannerErrors "Event delete" $ do
    action eventId
    pure $ do
      Lucid.toHtmlRaw ("" :: Text)
      renderBanner Success "Event Deleted" "The event has been deleted successfully."

--------------------------------------------------------------------------------
-- Helpers

fetchEvent ::
  Events.Id ->
  ExceptT HandlerError AppM Events.Model
fetchEvent eventId =
  fromMaybeM (throwNotFound "Event") $
    fromRightM throwDatabaseError $
      execQuery (Events.getEventById eventId)

deleteEvent ::
  Events.Model ->
  ExceptT HandlerError AppM ()
deleteEvent event = do
  result <-
    fromRightM throwDatabaseError $
      execQuery (Events.deleteEvent event.emId)
  case result of
    Nothing -> throwNotFound "Event"
    Just _ -> Log.logInfo "Event deleted successfully" event.emId
