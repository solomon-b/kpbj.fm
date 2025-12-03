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
    "GET /dashboard/:slug"
    ( "dashboard"
        :> Servant.Capture "slug" Slug
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Redirect /dashboard/:slug to /dashboard/episodes/:slug
handler ::
  (MonadIO m) =>
  Tracer ->
  Slug ->
  m (Lucid.Html ())
handler _tracer showSlug =
  let baseUrl = Links.linkURI $ dashboardEpisodesGetLink showSlug
   in pure $ redirectTemplate [i|/#{baseUrl}|]
