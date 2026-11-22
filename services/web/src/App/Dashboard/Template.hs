-- | Render a dashboard page inside the dashboard frame.
module App.Dashboard.Template
  ( renderDashboardTemplate,
  )
where

--------------------------------------------------------------------------------

import App.Dashboard.CurrentShow (fetchDashboardShows)
import App.Dashboard.ShowContext (ShowContext (..))
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav)
import Component.DashboardFrame qualified as DashboardFrame
import Component.Flash (FlashMessage)
import Control.Monad.Reader (asks)
import Data.Has (getter)
import Domain.Types.HxRequest (HxRequest (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

--------------------------------------------------------------------------------

-- | Render a dashboard page.
--
-- A full page request gets the dashboard frame. An HTMX request gets the
-- content, and the sidebar navigation and the top bar out of band. The
-- content of a page about a show has the cookie script in front of it.
--
-- The top bar needs the options of the show selector only for a page about a
-- show, so only that case runs the query. It runs for both request types.
renderDashboardTemplate ::
  HxRequest ->
  -- | The flash message of the request, from 'App.Flash.ReadFlash'. Only a
  -- full page shows it.
  Maybe FlashMessage ->
  UserMetadata.Model ->
  ShowContext ->
  DashboardNav ->
  -- | Stats for the top bar
  Maybe (Lucid.Html ()) ->
  -- | Action button for the top bar
  Maybe (Lucid.Html ()) ->
  Lucid.Html () ->
  AppM (Lucid.Html ())
renderDashboardTemplate hxRequest mFlash userMetadata showContext activeNav statsContent actionButton templateContent = do
  selectorShows <- case showContext of
    NoShow -> pure []
    ForShow _ -> fetchDashboardShows userMetadata
  case hxRequest of
    IsHxRequest ->
      pure $
        DashboardFrame.htmxResponse
          userMetadata
          showContext
          selectorShows
          activeNav
          statsContent
          actionButton
          templateContent
    IsNotHxRequest -> do
      mGoogleAnalyticsId <- asks getter
      DashboardFrame.loadDashboardFrame
        mGoogleAnalyticsId
        userMetadata
        mFlash
        showContext
        selectorShows
        activeNav
        statsContent
        actionButton
        templateContent
