module API.Shows.Slug.Feed.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Slug (Slug)
import Servant ((:>))
import Servant qualified
import Text.RSS (RSS)
import Text.XmlHtml qualified as Xml

--------------------------------------------------------------------------------

-- | "GET /shows/:show_slug/feed.xml"
--
-- The RSS feed of one show. It holds the published episodes of that show.
type Route =
  "shows"
    :> Servant.Capture "show_slug" Slug
    :> "feed.xml"
    :> Servant.Get '[RSS] (Servant.Headers '[Servant.Header "Cache-Control" Text] Xml.Document)
