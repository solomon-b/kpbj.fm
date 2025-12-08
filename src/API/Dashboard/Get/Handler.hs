{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (dashboardLinks)
import API.Types
import Component.Redirect (redirectTemplate)
import Control.Monad.IO.Class (MonadIO)
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
  (MonadIO m) =>
  Tracer ->
  m (Lucid.Html ())
handler _tracer =
  pure $ redirectTemplate [i|/#{dashboardEpisodesRedirectUrl}|]
