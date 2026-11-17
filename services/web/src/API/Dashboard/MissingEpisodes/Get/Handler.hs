{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.MissingEpisodes.Get.Handler (handler, action, MissingEpisodesViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.MissingEpisodes.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Missing episodes" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action userMetadata
    let pageTemplate = template vd.mevMissingEpisodes
    lift $ renderDashboardTemplate hxRequest vd.mevUserMetadata NoShow NavMissingEpisodes Nothing Nothing pageTemplate

--------------------------------------------------------------------------------

-- | All data needed to render the missing episodes page.
data MissingEpisodesViewData = MissingEpisodesViewData
  { mevUserMetadata :: UserMetadata.Model,
    mevMissingEpisodes :: [ShowSchedule.ShowMissingEpisode]
  }

-- | Business logic: missing episode fetching.
action ::
  UserMetadata.Model ->
  ExceptT HandlerError AppM MissingEpisodesViewData
action userMetadata = do
  -- 1. Fetch missing episodes
  missingEpisodes <-
    fromRightM throwDatabaseError $
      execQuery ShowSchedule.getShowsMissingEpisodes

  pure
    MissingEpisodesViewData
      { mevUserMetadata = userMetadata,
        mevMissingEpisodes = missingEpisodes
      }
