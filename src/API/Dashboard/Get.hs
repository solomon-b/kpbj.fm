{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (dashboardEpisodesGetLink)
import Component.Redirect (redirectTemplate)
import Control.Monad.IO.Class (MonadIO)
import Data.String.Interpolate (i)
import Domain.Types.Slug (Slug)
import Effects.Observability qualified as Observability
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard"
    ( "dashboard"
        :> Servant.QueryParam "show" Slug
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Redirect /dashboard to /dashboard/episodes
handler ::
  (MonadIO m) =>
  Tracer ->
  Maybe Slug ->
  m (Lucid.Html ())
handler _tracer maybeShowSlug =
  let baseUrl = Links.linkURI $ dashboardEpisodesGetLink maybeShowSlug
   in pure $ redirectTemplate [i|/#{baseUrl}|]
