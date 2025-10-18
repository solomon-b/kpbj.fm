{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Host.Dashboard.Get.Templates.Episode
  ( renderEpisodeTableRow,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodesDeleteLink, episodesEditGetLink)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Base qualified as LucidBase
import Lucid.Extras (hxDelete_, hxGet_, hxPushUrl_, hxSwap_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

episodeEditGetUrl :: Slug -> Slug -> Links.URI
episodeEditGetUrl showSlug episodeSlug = Links.linkURI $ episodesEditGetLink showSlug episodeSlug

episodeDeleteUrl :: Slug -> Slug -> Links.URI
episodeDeleteUrl showSlug episodeSlug = Links.linkURI $ episodesDeleteLink showSlug episodeSlug

--------------------------------------------------------------------------------

-- | Render individual episode as table row
renderEpisodeTableRow :: Shows.Model -> Episodes.Model -> Lucid.Html ()
renderEpisodeTableRow showModel episode = do
  let episodeId = episode.id
      episodeRowId = [i|episode-row-#{episodeId}|]
  Lucid.tr_ [Lucid.class_ "border-b border-gray-300 hover:bg-gray-50", Lucid.id_ episodeRowId] $ do
    -- Episode number
    Lucid.td_ [Lucid.class_ "px-4 py-3 font-bold text-gray-800"] $
      Lucid.toHtml $
        "#" <> Text.pack (show episode.episodeNumber)

    -- Title
    Lucid.td_ [Lucid.class_ "px-4 py-3"] $ do
      Lucid.div_ [Lucid.class_ "font-bold text-gray-900"] $ Lucid.toHtml episode.title
      case episode.description of
        Nothing -> mempty
        Just desc ->
          Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mt-1"] $ do
            Lucid.toHtml $ Text.take 100 desc
            if Text.length desc > 100 then "..." else ""

    -- Scheduled date
    Lucid.td_ [Lucid.class_ "px-4 py-3 text-sm"] $ do
      case episode.scheduledAt of
        Just scheduledAt -> Lucid.toHtml $ Text.pack $ formatTime defaultTimeLocale "%b %d, %Y" scheduledAt
        Nothing -> Lucid.span_ [Lucid.class_ "text-gray-500 italic"] "Not scheduled"

    -- Status
    Lucid.td_ [Lucid.class_ "px-4 py-3 text-sm"] $ do
      Lucid.span_ [Lucid.class_ "inline-block bg-green-100 text-green-800 px-2 py-1 rounded text-xs font-bold"] "PUBLISHED"

    -- Actions
    Lucid.td_ [Lucid.class_ "px-4 py-3 text-right"] $ do
      let episodeEditUrl = episodeEditGetUrl showModel.slug episode.slug
          episodeDelUrl = episodeDeleteUrl showModel.slug episode.slug
      Lucid.div_ [Lucid.class_ "flex gap-2 justify-end"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{episodeEditUrl}|],
            hxGet_ [i|/#{episodeEditUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-blue-600 text-white px-3 py-1 text-xs font-bold hover:bg-blue-700 no-underline"
          ]
          "EDIT"
        Lucid.button_
          [ hxDelete_ [i|/#{episodeDelUrl}|],
            hxTarget_ ("#" <> episodeRowId),
            hxSwap_ "outerHTML",
            LucidBase.makeAttributes "hx-confirm" "Are you sure you want to archive this episode? It will no longer appear in your dashboard or public show page.",
            Lucid.class_ "bg-red-600 text-white px-3 py-1 text-xs font-bold hover:bg-red-700"
          ]
          "ARCHIVE"
