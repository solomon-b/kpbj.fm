module API.Media.Get.Route where

--------------------------------------------------------------------------------

import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route = "media" :> Servant.Raw
