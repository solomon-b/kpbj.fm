module API.Feed.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Servant ((:>))
import Servant qualified
import Text.RSS (RSS)
import Text.XmlHtml qualified as Xml

--------------------------------------------------------------------------------

-- | "GET /feed.xml"
--
-- The station RSS feed. It holds the published episodes of every show.
type Route =
  "feed.xml"
    :> Servant.Get '[RSS] (Servant.Headers '[Servant.Header "Cache-Control" Text] Xml.Document)
