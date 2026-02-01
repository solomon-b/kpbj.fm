module API.Debug.Version.Get.Route where

--------------------------------------------------------------------------------

import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | "GET /debug/version"
type Route =
  "debug"
    :> "version"
    :> Servant.Get '[Servant.PlainText] String
