{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.MissingEpisodes.Past.Get.Handler (handler, action, PastViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.MissingEpisodes.Past.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)
import Domain.Attendance (AttendanceRange, defaultRange, rangeStart)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Satisfies 'UserMetadata.Staff RoleAuth ->
  Maybe AttendanceRange ->
  Maybe FlashMessage ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) mRange mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors "Missed episodes" apiLinks.rootGet $ do
    vd <- action auth.raMetadata (fromMaybe defaultRange mRange)
    let pageTemplate = template vd.pvdRange vd.pvdSlots
    lift $ renderDashboardTemplate hxRequest mFlash vd.pvdUserMetadata NoShow NavMissingEpisodes Nothing Nothing pageTemplate

--------------------------------------------------------------------------------

-- | All data needed to render the missed episodes page.
data PastViewData = PastViewData
  { pvdUserMetadata :: UserMetadata.Model,
    pvdRange :: AttendanceRange,
    -- | The finished slots of the range, newest first.
    pvdSlots :: [ShowSchedule.PastSlot]
  }

-- | Business logic: fetch the finished slots of the range.
action ::
  UserMetadata.Model ->
  AttendanceRange ->
  ExceptT HandlerError AppM PastViewData
action userMetadata range = do
  now <- liftIO getCurrentTime
  slots <-
    fromRightM throwDatabaseError $
      execQuery (ShowSchedule.getPastSlots (rangeStart now range) now)

  pure
    PastViewData
      { pvdUserMetadata = userMetadata,
        pvdRange = range,
        pvdSlots = slots
      }
