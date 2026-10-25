module API.Feed.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Feed.Get.Templates (template)
import App.BaseUrl (baseUrl)
import App.Monad (AppM)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Data.Has (getter)
import Data.Text (Text)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Clock (currentSystemTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes
import Log qualified
import Servant qualified
import Text.RSS (feedCacheControl)
import Text.XmlHtml qualified as Xml

--------------------------------------------------------------------------------

-- | Handler for the station RSS feed.
--
-- A database error gives a 500. The feed is XML, so the HTML error pages of
-- the site do not apply here.
handler :: AppM (Servant.Headers '[Servant.Header "Cache-Control" Text] Xml.Document)
handler = do
  currentTime <- currentSystemTime
  queryResult <- execQuery (Episodes.getFeedEpisodesWithShows currentTime)
  case queryResult of
    Left queryError -> do
      Log.logAttention "Station feed query failed" (show queryError)
      throwM Servant.err500
    Right episodesWithShows -> do
      backend <- asks (getter @StorageBackend)
      siteBaseUrl <- baseUrl
      let feed = template siteBaseUrl backend episodesWithShows
      pure (Servant.addHeader feedCacheControl feed)
