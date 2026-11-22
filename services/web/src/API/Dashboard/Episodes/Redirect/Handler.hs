{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Redirect.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.CurrentShow.Get.Templates (noShowsEmptyState)
import API.Links (apiLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.CurrentShow (resolveCurrentShow)
import App.Dashboard.ShowContext (ShowContext (..), ShowSection (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..), sectionUrl)
import Component.Flash (throwHxRedirect)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | Business logic: find the current show.
action ::
  Maybe Cookie ->
  UserMetadata.Model ->
  ExceptT HandlerError AppM (Maybe Shows.Model)
action cookie userMetadata = lift $ resolveCurrentShow cookie userMetadata

-- | Handler that redirects to the episodes page of the current show.
handler ::
  Satisfies 'UserMetadata.Host RoleAuth ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Episodes redirect" apiLinks.rootGet $ do
    mCurrentShow <- action cookie auth.raMetadata
    case mCurrentShow of
      Nothing ->
        -- No shows available, so render the dashboard with an empty state.
        lift $ renderDashboardTemplate hxRequest auth.raMetadata NoShow NavEpisodes Nothing Nothing noShowsEmptyState
      Just currentShow -> do
        Log.logInfo "Redirecting to the episodes page of the current show" currentShow.slug
        let episodesUrl = sectionUrl SectionEpisodes currentShow
        lift $ throwHxRedirect [i|/#{episodesUrl}|] Nothing
