{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Host.Dashboard.Get.Templates.Episode
  ( renderEpisodeTableRow,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodesDeleteLink, episodesEditGetLink, episodesGetLink)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Base qualified as LucidBase
import Lucid.Extras (hxDelete_, hxGet_, hxPushUrl_, hxSwap_, hxTarget_, xData_, xOnChange_, xRef_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

episodeGetUrl :: Shows.Id -> Episodes.Id -> Slug -> Links.URI
episodeGetUrl showId episodeId episodeSlug = Links.linkURI $ episodesGetLink showId episodeId episodeSlug

episodeEditGetUrl :: Shows.Id -> Episodes.Id -> Slug -> Links.URI
episodeEditGetUrl showId episodeId episodeSlug = Links.linkURI $ episodesEditGetLink showId episodeId episodeSlug

episodeDeleteUrl :: Shows.Id -> Slug -> Episodes.Id -> Slug -> Links.URI
episodeDeleteUrl showId showSlug episodeId episodeSlug = Links.linkURI $ episodesDeleteLink showId showSlug episodeId episodeSlug

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
      let episodeUrl = episodeGetUrl showModel.id episode.id episode.slug
      Lucid.div_ [Lucid.class_ "font-bold text-gray-900"]
        $ Lucid.a_
          [ Lucid.href_ [i|/#{episodeUrl}|],
            hxGet_ [i|/#{episodeUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "hover:underline text-blue-600"
          ]
        $ Lucid.toHtml episode.title
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
      case episode.status of
        Episodes.Draft ->
          Lucid.span_ [Lucid.class_ "inline-block bg-yellow-100 text-yellow-800 px-2 py-1 rounded text-xs font-bold"] "DRAFT"
        Episodes.Published ->
          Lucid.span_ [Lucid.class_ "inline-block bg-green-100 text-green-800 px-2 py-1 rounded text-xs font-bold"] "PUBLISHED"
        Episodes.Deleted ->
          Lucid.span_ [Lucid.class_ "inline-block bg-red-100 text-red-800 px-2 py-1 rounded text-xs font-bold"] "DELETED"

    -- Actions dropdown
    Lucid.td_ [Lucid.class_ "px-4 py-3 text-center"]
      $ Lucid.div_ [xData_ "{}"]
      $ do
        -- Hidden link for Edit - HTMX handles history properly
        Lucid.a_
          [ Lucid.href_ [i|/#{episodeEditUrl}|],
            hxGet_ [i|/#{episodeEditUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            xRef_ "editLink",
            Lucid.class_ "hidden"
          ]
          ""
        -- Hidden button for Archive
        Lucid.button_
          [ hxDelete_ [i|/#{episodeDelUrl}|],
            hxTarget_ ("#" <> episodeRowId),
            hxSwap_ "outerHTML",
            LucidBase.makeAttributes "hx-confirm" "Are you sure you want to archive this episode? It will no longer appear in your dashboard or public show page.",
            xRef_ "archiveBtn",
            Lucid.class_ "hidden"
          ]
          ""
        -- Visible dropdown
        Lucid.select_
          [ Lucid.class_ "p-2 border border-gray-400 text-xs bg-white",
            xOnChange_
              [i|
              const action = $el.value;
              $el.value = '';
              if (action === 'edit') {
                $refs.editLink.click();
              } else if (action === 'archive') {
                $refs.archiveBtn.click();
              }
            |]
          ]
          $ do
            Lucid.option_ [Lucid.value_ ""] "Actions..."
            Lucid.option_ [Lucid.value_ "edit"] "Edit"
            Lucid.option_ [Lucid.value_ "archive"] "Archive"
  where
    episodeEditUrl = episodeEditGetUrl showModel.id episode.id episode.slug
    episodeDelUrl = episodeDeleteUrl showModel.id showModel.slug episode.id episode.slug
