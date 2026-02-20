{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.New.Get.Handler (handler, action, NewEventViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.New.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

--------------------------------------------------------------------------------

-- | All data needed to render the new event form.
data NewEventViewData = NewEventViewData
  { nevUserMetadata :: UserMetadata.Model,
    nevAllShows :: [Shows.Model],
    nevSelectedShow :: Maybe Shows.Model
  }

-- | Business logic: fetch shows for sidebar.
action ::
  User.Model ->
  UserMetadata.Model ->
  ExceptT HandlerError AppM NewEventViewData
action user userMetadata = do
  -- 1. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
      selectedShow = listToMaybe allShows

  pure
    NewEventViewData
      { nevUserMetadata = userMetadata,
        nevAllShows = allShows,
        nevSelectedShow = selectedShow
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Event create form" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to create events." userMetadata
    vd <- action user userMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        vd.nevUserMetadata
        vd.nevAllShows
        vd.nevSelectedShow
        NavEvents
        Nothing
        Nothing
        (template vd.nevUserMetadata)
