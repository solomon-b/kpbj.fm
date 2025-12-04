{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (dashboardEpisodesRedirectLink)
import Component.Redirect (redirectTemplate)
import Control.Monad.IO.Class (MonadIO)
import Data.String.Interpolate (i)
import Effects.Observability qualified as Observability
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
dashboardEpisodesRedirectUrl :: Links.URI
dashboardEpisodesRedirectUrl = Links.linkURI dashboardEpisodesRedirectLink

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard"
    ( "dashboard"
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Redirect /dashboard to /dashboard/episodes
handler ::
  (MonadIO m) =>
  Tracer ->
  m (Lucid.Html ())
handler _tracer =
  pure $ redirectTemplate [i|/#{dashboardEpisodesRedirectUrl}|]
