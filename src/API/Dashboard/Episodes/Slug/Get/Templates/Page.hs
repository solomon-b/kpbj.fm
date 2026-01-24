{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Episodes.Slug.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEpisodesLinks)
import API.Types
import Component.AudioPlayer.Waveform qualified as WaveformPlayer
import Control.Monad (unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.EpisodeTags qualified as EpisodeTags
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardEpisodesGetUrl :: Slug -> Links.URI
dashboardEpisodesGetUrl showSlug = Links.linkURI $ dashboardEpisodesLinks.list showSlug Nothing

--------------------------------------------------------------------------------

-- | Dashboard episode detail template
template :: StorageBackend -> UserMetadata.Model -> Shows.Model -> Episodes.Model -> [EpisodeTrack.Model] -> [EpisodeTags.Model] -> Lucid.Html ()
template backend _userMeta showModel episode tracks tags = do
  -- Back button and header
  Lucid.div_ [class_ $ base [Tokens.mb6]] $ do
    let backUrl = dashboardEpisodesGetUrl showModel.slug
    Lucid.a_
      [ Lucid.href_ [i|/#{backUrl}|],
        hxGet_ [i|/#{backUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base [Tokens.textGray600, "hover:text-gray-800 dark:hover:text-white", Tokens.textSm, "inline-flex", "items-center", Tokens.gap2]
      ]
      $ do
        Lucid.i_ [Lucid.class_ "fa-solid fa-arrow-left"] mempty
        "Back to Episodes"

  -- Main episode container
  Lucid.div_ [class_ $ base [Tokens.bgWhite, "rounded"]] $ do
    -- Episode header with artwork
    Lucid.div_ [class_ $ base ["border-b", Tokens.borderGray600, Tokens.p6]] $ do
      Lucid.div_ [class_ $ base ["flex", Tokens.gap6, Tokens.mb6]] $ do
        -- Episode artwork
        Lucid.div_ [class_ $ base ["w-48", "h-48", "bg-gray-300 dark:bg-gray-600", "border-2", "border-gray-600 dark:border-gray-500", "flex", "items-center", "justify-center", "text-xs", "flex-shrink-0"]] $ do
          case episode.artworkUrl of
            Just artworkPath ->
              Lucid.img_
                [ Lucid.src_ (buildMediaUrl backend artworkPath),
                  Lucid.alt_ "Episode artwork",
                  class_ $ base [Tokens.fullWidth, "h-full", "object-cover"]
                ]
            Nothing -> "[EPISODE ARTWORK]"

        -- Episode metadata
        Lucid.div_ [Lucid.class_ "flex-grow"] $ do
          Lucid.div_ $ do
            let epNum = episode.episodeNumber
            Lucid.div_ [class_ $ base ["text-xs", "uppercase", "tracking-wide", Tokens.textGray600, Tokens.mb2]] $
              "Episode " <> Lucid.toHtml (show epNum)
            Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4]] $ Lucid.toHtml (show epNum)

          -- Episode info grid
          Lucid.div_ [class_ $ base ["grid", "grid-cols-2", Tokens.gap4, Tokens.textSm, Tokens.mb4]] $ do
            -- Status
            Lucid.div_ $ do
              Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray600]] "Status: "
              case episode.status of
                Episodes.Draft ->
                  Lucid.span_ [class_ $ base ["inline-block", "bg-yellow-100", "text-yellow-800", "px-2", "py-1", "rounded", "text-xs", Tokens.fontBold]] "DRAFT"
                Episodes.Published ->
                  Lucid.span_ [class_ $ base ["inline-block", "bg-green-100", "text-green-800", "px-2", "py-1", "rounded", "text-xs", Tokens.fontBold]] "PUBLISHED"
                Episodes.Deleted ->
                  Lucid.span_ [class_ $ base ["inline-block", "bg-red-100", "text-red-800", "px-2", "py-1", "rounded", "text-xs", Tokens.fontBold]] "DELETED"

            -- Aired/Scheduled date
            Lucid.div_ $ do
              Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray600]] "Scheduled: "
              let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" episode.scheduledAt
              Lucid.toHtml dateStr

            -- Published date
            case episode.publishedAt of
              Just publishedAt -> do
                Lucid.div_ $ do
                  Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray600]] "Published: "
                  let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
                  Lucid.toHtml dateStr
              Nothing -> mempty

            -- Duration
            case episode.durationSeconds of
              Just duration -> do
                let hours = duration `div` 3600
                    minutes = (duration `mod` 3600) `div` 60
                Lucid.div_ $ do
                  Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray600]] "Duration: "
                  if hours > 0
                    then Lucid.toHtml (show hours) <> "h " <> Lucid.toHtml (show minutes) <> "min"
                    else Lucid.toHtml (show minutes) <> "min"
              Nothing -> mempty

          -- Episode description
          case episode.description of
            Just desc -> Lucid.p_ [class_ $ base [Tokens.textGray600, "leading-relaxed"]] $ Lucid.toHtml desc
            Nothing -> mempty

          -- Episode tags
          unless (null tags) $ do
            Lucid.div_ [class_ $ base [Tokens.mt4]] $ do
              Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray600, "mr-2"]] "Tags:"
              Lucid.div_ [class_ $ base ["inline-flex", "flex-wrap", Tokens.gap2]] $ do
                mapM_ renderTag tags

    -- Audio player section
    case episode.audioFilePath of
      Just audioPath -> do
        let episodeNum = episode.episodeNumber
            episodeIdVal = episode.id
            audioUrl :: Text
            audioUrl = buildMediaUrl backend audioPath
            playerId :: Text
            playerId = [i|dashboard-episode-#{episodeIdVal}|]
            showTitle = showModel.title
            episodeMetadata :: Text
            episodeMetadata = [i|#{showTitle} - Episode #{episodeNum}|]

        Lucid.div_ [class_ $ base ["border-b", Tokens.borderGray600, Tokens.p6]] $ do
          Lucid.h2_ [class_ $ base [Tokens.textLg, Tokens.fontBold, Tokens.mb4, "uppercase"]] "Preview Audio"
          WaveformPlayer.render
            WaveformPlayer.Config
              { WaveformPlayer.playerId = playerId,
                WaveformPlayer.audioUrl = audioUrl,
                WaveformPlayer.title = episodeMetadata,
                WaveformPlayer.fileInputId = Nothing,
                WaveformPlayer.containerClasses = Nothing,
                WaveformPlayer.buttonClasses = Nothing,
                WaveformPlayer.progressBarClasses = Nothing,
                WaveformPlayer.progressFillClasses = Nothing,
                WaveformPlayer.timeDisplayClasses = Nothing
              }
      Nothing -> mempty

    -- Track listing section
    unless (null tracks) $ do
      Lucid.div_ [class_ $ base [Tokens.p6]] $ do
        Lucid.h2_ [class_ $ base [Tokens.textLg, Tokens.fontBold, Tokens.mb4, "uppercase", "border-b", Tokens.borderGray600, "pb-2"]] $
          "Track Listing (" <> Lucid.toHtml (show (length tracks)) <> " tracks)"

        Lucid.div_ [Lucid.class_ "space-y-1"] $ do
          mapM_ renderTrackRow tracks

--------------------------------------------------------------------------------

renderTag :: EpisodeTags.Model -> Lucid.Html ()
renderTag tag =
  Lucid.span_
    [class_ $ base [Tokens.bgGray100, Tokens.textGray600, "px-2", "py-1", "text-xs", "font-mono", "rounded"]]
    $ Lucid.toHtml
    $ "#" <> EpisodeTags.etName tag

--------------------------------------------------------------------------------

renderTrackRow :: EpisodeTrack.Model -> Lucid.Html ()
renderTrackRow track = do
  Lucid.div_ [class_ $ base ["flex", "justify-between", "items-start", "p-3", Tokens.hoverBgGray100, "border-b", Tokens.borderGray600]] $ do
    -- Track number
    Lucid.div_ [class_ $ base ["w-8", "flex-shrink-0", Tokens.textGray600, "font-mono", Tokens.textSm]] $
      Lucid.toHtml (show track.trackNumber <> ".")

    -- Track info
    Lucid.div_ [Lucid.class_ "flex-grow"] $ do
      Lucid.div_ [class_ $ base ["font-medium", Tokens.textGray800]] $ do
        "\"" <> Lucid.toHtml track.title <> "\""

      Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.textGray600]] $ do
        Lucid.toHtml track.artist
