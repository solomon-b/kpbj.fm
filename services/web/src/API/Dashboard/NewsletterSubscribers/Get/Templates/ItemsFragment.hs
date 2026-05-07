{-# LANGUAGE QuasiQuotes #-}

-- | Fragment template for infinite scroll append responses.
--
-- Renders only the new subscriber rows and the sentinel/end indicator,
-- without the page wrapper. Used when appending content via HTMX.
module API.Dashboard.NewsletterSubscribers.Get.Templates.ItemsFragment
  ( renderItemsFragment,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.NewsletterSubscribers.Get.Templates.Page (renderSubscriberRow)
import API.Links (dashboardNewsletterSubscribersLinks)
import API.Types
import Component.Table (renderTableFragment)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render just the subscriber rows and sentinel for infinite scroll append.
renderItemsFragment ::
  [NewsletterSubscribers.Model] ->
  Int64 ->
  Bool ->
  Maybe Text ->
  Lucid.Html ()
renderItemsFragment subscribers currentPage hasMore mSearch =
  renderTableFragment
    4
    "#subscribers-table-body"
    (if hasMore then Just [i|/#{nextPageUrl}|] else Nothing)
    (mapM_ renderSubscriberRow subscribers)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardNewsletterSubscribersLinks.list mSearch (Just (currentPage + 1))
