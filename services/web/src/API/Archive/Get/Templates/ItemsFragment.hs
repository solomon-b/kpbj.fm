{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Fragment template for infinite-scroll append responses.
--
-- Renders only the new episode cards and the next sentinel/end marker,
-- appended into @#archive-episodes-list@ via HTMX.
module API.Archive.Get.Templates.ItemsFragment
  ( renderItemsFragment,
  )
where

--------------------------------------------------------------------------------

import API.Links (archiveLink)
import Component.Card.Episode (renderEpisodeCard)
import Component.InfiniteScroll (renderEndOfContent, renderSentinel)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

renderItemsFragment ::
  StorageBackend ->
  [(Episodes.Model, Shows.Model)] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
renderItemsFragment backend episodes currentPage hasMore = do
  traverse_ (\(ep, s) -> renderEpisodeCard backend True s ep) episodes
  if hasMore
    then renderSentinel [i|/#{nextPageUrl}|] "#archive-episodes-list"
    else renderEndOfContent
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ archiveLink (Just (currentPage + 1))
