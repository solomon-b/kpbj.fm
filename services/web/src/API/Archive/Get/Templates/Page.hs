{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Archive.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (archiveLink)
import Component.Card.Episode (renderEpisodeCard)
import Component.InfiniteScroll (renderEndOfContent, renderLoadingIndicator, renderSentinel)
import Component.PageHeader (pageHeader)
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Design (base, class_, desktop, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Archive page: a grid of published episodes across all shows with
-- infinite scroll, mirroring the blog list page.
template ::
  StorageBackend ->
  [(Episodes.Model, Shows.Model)] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
template backend episodes currentPage hasMore = do
  pageHeader "ARCHIVE"

  Lucid.section_ [Lucid.id_ "archive-episodes-content-container", Lucid.class_ "w-full"] $ do
    -- Grid container with stable ID for HTMX appending
    Lucid.div_
      [ Lucid.id_ "archive-episodes-list",
        class_ $ do base ["grid", "grid-cols-1", Tokens.gap6]; tablet ["grid-cols-2"]; desktop ["grid-cols-3"]
      ]
      $ if null episodes
        then Lucid.div_ [class_ $ base [Tokens.cardBase, "col-span-full", "text-center"]] $ do
          Lucid.h2_ [class_ $ base [Tokens.headingLg, Tokens.mb4]] "No Episodes Yet"
          Lucid.p_ [class_ $ base [Tokens.fgMuted]] "Check back soon for new episodes from the KPBJ community!"
        else traverse_ (\(ep, s) -> renderEpisodeCard backend True s ep) episodes

    -- Loading indicator (hidden by default, shown during HTMX requests)
    renderLoadingIndicator

    -- Sentinel for infinite scroll or end indicator
    unless (null episodes) $
      if hasMore
        then renderSentinel [i|/#{nextPageUrl}|] "#archive-episodes-list"
        else renderEndOfContent
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ archiveLink (Just (currentPage + 1))
