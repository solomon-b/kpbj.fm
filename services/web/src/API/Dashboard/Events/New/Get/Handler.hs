{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.New.Get.Handler (handler, action, NewEventViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.New.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

--------------------------------------------------------------------------------

-- | All data needed to render the new event form.
newtype NewEventViewData = NewEventViewData
  { nevUserMetadata :: UserMetadata.Model
  }

-- | Business logic: build the view data. This page needs no queries.
action ::
  UserMetadata.Model ->
  ExceptT HandlerError AppM NewEventViewData
action userMetadata =
  pure
    NewEventViewData
      { nevUserMetadata = userMetadata
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Event create form" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to create events." userMetadata
    vd <- action userMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        vd.nevUserMetadata
        NoShow
        NavEvents
        Nothing
        Nothing
        (template vd.nevUserMetadata)
