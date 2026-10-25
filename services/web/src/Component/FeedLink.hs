{-# LANGUAGE QuasiQuotes #-}

-- | RSS discovery links for the page head.
--
-- A feed reader that gets a page URL looks in the head for a
-- @\<link rel="alternate" type="application/rss+xml"\>@ tag to find the feed.
-- A handler gives a list of 'FeedLink' values to
-- 'App.Common.renderTemplateWithFeeds', and the frame puts one tag in the head
-- for each value.
module Component.FeedLink
  ( FeedLink (..),
    stationFeed,
    showFeed,
    renderFeedLink,
  )
where

--------------------------------------------------------------------------------

import API.Links (rootLink, showFeedLink, stationFeedLink)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified

--------------------------------------------------------------------------------

-- | One feed that a page offers.
data FeedLink = FeedLink
  { -- | The name that a feed reader shows for the feed.
    feedLinkTitle :: Text,
    -- | The URL of the feed.
    feedLinkHref :: Text
  }
  deriving stock (Show, Eq)

-- | The station feed, with the episodes of every show.
stationFeed :: FeedLink
stationFeed =
  FeedLink
    { feedLinkTitle = "KPBJ 95.9FM",
      feedLinkHref = rootLink stationFeedLink
    }

-- | The feed of one show.
showFeed :: Shows.Model -> FeedLink
showFeed showModel =
  FeedLink
    { feedLinkTitle = [i|#{showTitle} - KPBJ 95.9FM|],
      feedLinkHref = rootLink (showFeedLink showModel.slug)
    }
  where
    showTitle = showModel.title

-- | The @link@ tag for one feed.
renderFeedLink :: FeedLink -> Lucid.Html ()
renderFeedLink feedLink =
  Lucid.link_
    [ Lucid.rel_ "alternate",
      Lucid.type_ "application/rss+xml",
      Lucid.title_ feedLink.feedLinkTitle,
      Lucid.href_ feedLink.feedLinkHref
    ]
