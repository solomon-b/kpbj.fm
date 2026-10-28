{-# LANGUAGE QuasiQuotes #-}

module API.Feed.Get.Templates (template) where

--------------------------------------------------------------------------------

import API.Links (apiLinks, rootLink, stationFeedLink)
import API.Types (Routes (..))
import Component.EpisodeFeedItem (episodeFeedItem, episodeTitle)
import Data.Maybe (mapMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Text.RSS qualified as RSS
import Text.XmlHtml qualified as Xml

--------------------------------------------------------------------------------

-- | The station RSS feed.
--
-- The episodes come newest first, so the first item gives the
-- @lastBuildDate@.
template ::
  -- | The site base URL, from 'App.BaseUrl.baseUrl'
  Text ->
  StorageBackend ->
  -- | Each episode with its show and the instant it aired
  [(Episodes.Model, Shows.Model, UTCTime)] ->
  Xml.Document
template siteBaseUrl backend episodesWithShows =
  RSS.renderFeed channel items
  where
    items = mapMaybe toItem episodesWithShows

    toItem (episode, showModel, airedAt) =
      episodeFeedItem siteBaseUrl backend (stationItemTitle showModel episode) showModel episode airedAt

    newestPublishedAt =
      case items of
        [] -> Nothing
        newestItem : _ -> Just newestItem.itemPublishedAt

    channel =
      RSS.Channel
        { RSS.channelTitle = "KPBJ 95.9FM",
          RSS.channelLink = siteBaseUrl <> rootLink apiLinks.rootGet,
          RSS.channelDescription = "Episodes from every show on KPBJ 95.9FM.",
          RSS.channelSelfUrl = siteBaseUrl <> rootLink stationFeedLink,
          RSS.channelImageUrl = Nothing,
          RSS.channelLastBuildDate = newestPublishedAt
        }

-- | The station feed mixes all shows, so each title starts with the show.
--
-- This is the same title that the audio player shows for an episode.
stationItemTitle :: Shows.Model -> Episodes.Model -> Text
stationItemTitle showModel episode =
  [i|#{showTitle} - #{episodeTitle episode}|]
  where
    showTitle = showModel.title
