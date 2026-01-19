module API.Debug.Version.Get.Route where

--------------------------------------------------------------------------------

import Effects.Observability qualified as Observability
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /debug/version"
    ( "debug"
        :> "version"
        :> Servant.Get '[Servant.PlainText] String
    )
