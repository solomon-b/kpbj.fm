module API.Static.Get.Route where

--------------------------------------------------------------------------------

import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route = "static" :> Servant.Raw
