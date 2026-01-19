module API.Debug.Version.Get.Handler where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import Data.Version (showVersion)
import OpenTelemetry.Trace (Tracer)
import Paths_kpbj_api (version)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  AppM String
handler _tracer = pure $ showVersion version
