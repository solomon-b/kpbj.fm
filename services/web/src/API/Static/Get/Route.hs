module API.Static.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | "GET /static/:filename"
--
-- Serves compile-time embedded static assets by filename.
-- Uses 'Servant.Raw' so the handler controls the response directly,
-- including the Content-Type header — no Servant content negotiation.
type Route =
  "static"
    :> Servant.Capture "filename" Text
    :> Servant.Raw
