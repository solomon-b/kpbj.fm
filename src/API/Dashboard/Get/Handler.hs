{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (dashboardLinks)
import API.Types
import App.Monad (AppM)
import Component.Redirect (redirectTemplate)
import Data.String.Interpolate (i)
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardEpisodesRedirectUrl :: Links.URI
dashboardEpisodesRedirectUrl = Links.linkURI dashboardLinks.episodesRedirect

--------------------------------------------------------------------------------

-- | Redirect /dashboard to /dashboard/episodes
handler ::
  Tracer ->
  AppM (Lucid.Html ())
handler _tracer =
  pure $ redirectTemplate [i|/#{dashboardEpisodesRedirectUrl}|]
