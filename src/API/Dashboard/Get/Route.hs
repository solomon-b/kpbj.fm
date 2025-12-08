module API.Dashboard.Get.Route where

--------------------------------------------------------------------------------

import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard"
    ( "dashboard"
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
