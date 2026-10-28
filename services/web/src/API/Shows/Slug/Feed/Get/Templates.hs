{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Feed.Get.Templates (template) where

--------------------------------------------------------------------------------

import API.Links (rootLink, showFeedLink, showsLinks)
import API.Types (ShowsRoutes (..))
import App.BaseUrl (absoluteMediaUrl)
import Component.EpisodeFeedItem (episodeFeedItem, episodeTitle)
import Data.Maybe (mapMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Text.RSS qualified as RSS
import Text.XmlHtml qualified as Xml

--------------------------------------------------------------------------------

-- | The RSS feed of one show.
--
-- The episodes come newest first, so the first item gives the
-- @lastBuildDate@.
template ::
  -- | The site base URL, from 'App.BaseUrl.baseUrl'
  Text ->
  StorageBackend ->
  Shows.Model ->
  -- | Each episode with the instant it aired
  [(Episodes.Model, UTCTime)] ->
  Xml.Document
template siteBaseUrl backend showModel episodes =
  RSS.renderFeed channel items
  where
    items = mapMaybe toItem episodes

    toItem (episode, airedAt) =
      episodeFeedItem siteBaseUrl backend (episodeTitle episode) showModel episode airedAt

    newestPublishedAt =
      case items of
        [] -> Nothing
        newestItem : _ -> Just newestItem.itemPublishedAt

    channel =
      RSS.Channel
        { RSS.channelTitle = showModel.title,
          RSS.channelLink = siteBaseUrl <> rootLink (showsLinks.detail showModel.slug Nothing),
          RSS.channelDescription = showDescription showModel,
          RSS.channelSelfUrl = siteBaseUrl <> rootLink (showFeedLink showModel.slug),
          RSS.channelImageUrl = fmap (absoluteMediaUrl siteBaseUrl backend) showModel.logoUrl,
          RSS.channelLastBuildDate = newestPublishedAt
        }

-- | The channel description. RSS 2.0 requires one, so a show with no
-- description gets a short default.
showDescription :: Shows.Model -> Text
showDescription showModel =
  case showModel.description of
    Just description
      | not (Text.null (Text.strip description)) -> description
    _ -> [i|Episodes of #{showTitle} on KPBJ 95.9FM.|]
  where
    showTitle = showModel.title
