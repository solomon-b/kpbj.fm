module API.Static.RangePng.Get.Route where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Effects.Observability qualified as Observability
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | Content type for PNG images.
data PNG

instance Servant.Accept PNG where
  contentType _ = "image/png"

instance Servant.MimeRender PNG ByteString where
  mimeRender _ = LBS.fromStrict

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /static/range.png"
    ( "static"
        :> "range.png"
        :> Servant.Get '[PNG] ByteString
    )
