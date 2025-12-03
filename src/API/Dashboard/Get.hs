module API.Dashboard.Get where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO)
import Effects.Observability qualified as Observability
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard/:slug"
    ( "dashboard"
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Redirect /dashboard/:slug to /dashboard/episodes
handler ::
  (MonadIO m) =>
  Tracer ->
  m (Lucid.Html ())
handler _tracer =
  pure mempty
