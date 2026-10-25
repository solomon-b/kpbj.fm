module API.Shows.Slug.Feed.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Shows.Slug.Feed.Get.Templates (template)
import App.BaseUrl (baseUrl)
import App.Monad (AppM)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Data.Has (getter)
import Data.Text (Text)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Clock (currentSystemTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Log qualified
import Servant qualified
import Text.RSS (feedCacheControl)
import Text.XmlHtml qualified as Xml

--------------------------------------------------------------------------------

-- | Handler for the RSS feed of one show.
--
-- An unknown slug or a deleted show gives a 404. A database error gives a
-- 500. The feed is XML, so the HTML error pages of the site do not apply here.
handler ::
  Slug ->
  AppM (Servant.Headers '[Servant.Header "Cache-Control" Text] Xml.Document)
handler showSlug = do
  showResult <- execQuery (Shows.getShowBySlug showSlug)
  case showResult of
    Left queryError -> failWithDatabaseError queryError
    Right Nothing -> throwM Servant.err404
    Right (Just showModel) -> do
      currentTime <- currentSystemTime
      episodesResult <- execQuery (Episodes.getFeedEpisodesForShow currentTime showModel.id)
      case episodesResult of
        Left queryError -> failWithDatabaseError queryError
        Right episodes -> do
          backend <- asks (getter @StorageBackend)
          siteBaseUrl <- baseUrl
          let feed = template siteBaseUrl backend showModel episodes
          pure (Servant.addHeader feedCacheControl feed)
  where
    failWithDatabaseError queryError = do
      Log.logAttention "Show feed query failed" (show queryError)
      throwM Servant.err500
