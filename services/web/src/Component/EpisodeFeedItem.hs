{-# LANGUAGE QuasiQuotes #-}

-- | Turn an episode into one item of an RSS feed.
--
-- The station feed and the show feeds both use this, so an episode looks the
-- same in each feed. Only the title differs, and the caller gives it.
module Component.EpisodeFeedItem
  ( episodeFeedItem,
    episodeTitle,
  )
where

--------------------------------------------------------------------------------

import API.Links (rootLink, showEpisodesLinks)
import API.Types (ShowEpisodesRoutes (..))
import App.BaseUrl (absoluteMediaUrl)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Text.RSS qualified as RSS

--------------------------------------------------------------------------------

-- | The title of an episode without its show, for example @Episode 12@.
episodeTitle :: Episodes.Model -> Text
episodeTitle episode = [i|Episode #{episodeNumber}|]
  where
    episodeNumber = episode.episodeNumber

-- | Build the feed item for one episode.
--
-- The item gives the air instant as its @pubDate@. Feed readers sort on
-- @pubDate@, so the upload time would put the episodes out of air order.
--
-- Gives 'Nothing' when the episode has no audio. The feed queries already
-- remove those episodes, but the column can hold NULL, so this function makes
-- the decision again instead of failing.
episodeFeedItem ::
  -- | The site base URL, from 'App.BaseUrl.baseUrl'
  Text ->
  StorageBackend ->
  -- | The title of the item
  Text ->
  Shows.Model ->
  Episodes.Model ->
  -- | The instant the episode aired
  UTCTime ->
  Maybe RSS.Item
episodeFeedItem siteBaseUrl backend title showModel episode airedAt =
  case episode.audioFilePath of
    Just audioPath ->
      Just
        RSS.Item
          { RSS.itemTitle = title,
            RSS.itemLink = episodePageUrl,
            RSS.itemDescription = episode.description,
            RSS.itemPublishedAt = airedAt,
            RSS.itemEnclosure =
              RSS.Enclosure
                { RSS.enclosureUrl = absoluteMediaUrl siteBaseUrl backend audioPath,
                  RSS.enclosureLength = audioFileSize,
                  RSS.enclosureType = audioMimeType
                }
          }
    Nothing -> Nothing
  where
    episodePageUrl =
      siteBaseUrl <> rootLink (showEpisodesLinks.detail showModel.slug episode.episodeNumber)

    -- RSS 2.0 requires a length. Zero is the usual value when the size is not
    -- known, and feed readers accept it.
    audioFileSize = fromMaybe 0 episode.audioFileSize

    -- RSS 2.0 requires a type. When the type is not known, use the MP3 type.
    -- MP3 is the type that most feed readers play.
    audioMimeType = fromMaybe "audio/mpeg" episode.audioMimeType
