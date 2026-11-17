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
import Control.Monad.Reader (asks)
import Data.Has (getter)
import Domain.Types.HxRequest (HxRequest (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

--------------------------------------------------------------------------------

-- | Render a dashboard page.
--
-- An HTMX request gets the content only and runs no query. The content of a
-- page about a show has the cookie script in front of it. A full page
-- request gets the dashboard frame. The frame needs the options of the show
-- selector only for a page about a show, so only that case runs the query.
renderDashboardTemplate ::
  HxRequest ->
  UserMetadata.Model ->
  ShowContext ->
  DashboardNav ->
  -- | Stats for the top bar
  Maybe (Lucid.Html ()) ->
  -- | Action button for the top bar
  Maybe (Lucid.Html ()) ->
  Lucid.Html () ->
  AppM (Lucid.Html ())
renderDashboardTemplate hxRequest userMetadata showContext activeNav statsContent actionButton templateContent =
  case hxRequest of
    IsHxRequest ->
      DashboardFrame.loadDashboardContentOnly showContext templateContent
    IsNotHxRequest -> do
      mGoogleAnalyticsId <- asks getter
      selectorShows <- case showContext of
        NoShow -> pure []
        ForShow _ -> fetchDashboardShows userMetadata
      DashboardFrame.loadDashboardFrame
        mGoogleAnalyticsId
        userMetadata
        showContext
        selectorShows
        activeNav
        statsContent
        actionButton
        templateContent
