module API.Static.RangePng.Get.Handler where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import Assets.Embedded qualified as Assets
import Data.ByteString (ByteString)
import OpenTelemetry.Trace qualified as Trace

--------------------------------------------------------------------------------

handler :: Trace.Tracer -> AppM ByteString
handler _tracer = pure Assets.rangePng
