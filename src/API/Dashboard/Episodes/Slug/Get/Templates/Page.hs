{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Episodes.Slug.Get.Templates.Page
  ( template,
    errorTemplate,
    notFoundTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, dashboardEpisodesLinks)
import API.Types
import Control.Monad (unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindStyle_, xData_, xOnClick_, xRef_, xText_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

dashboardEpisodesGetUrl :: Slug -> Links.URI
dashboardEpisodesGetUrl showSlug = Links.linkURI $ dashboardEpisodesLinks.list showSlug

--------------------------------------------------------------------------------

-- | Dashboard episode detail template
template :: UserMetadata.Model -> Shows.Model -> Episodes.Model -> [EpisodeTrack.Model] -> Lucid.Html ()
template _userMeta showModel episode tracks = do
  -- Back button and header
  Lucid.div_ [class_ $ base [Tokens.mb6]] $ do
    let backUrl = dashboardEpisodesGetUrl showModel.slug
    Lucid.a_
      [ Lucid.href_ [i|/#{backUrl}|],
        hxGet_ [i|/#{backUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base [Tokens.textGray600, "hover:text-gray-900", Tokens.textSm, "inline-flex", "items-center", Tokens.gap2]
      ]
      $ do
        Lucid.i_ [Lucid.class_ "fa-solid fa-arrow-left"] mempty
        "Back to Episodes"

  -- Main episode container
  Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder]] $ do
    -- Episode header with artwork
    Lucid.div_ [class_ $ base ["border-b-2", "border-gray-800", Tokens.p6]] $ do
      Lucid.div_ [class_ $ base ["flex", Tokens.gap6, Tokens.mb6]] $ do
        -- Episode artwork
        Lucid.div_ [class_ $ base ["w-48", "h-48", "bg-gray-300", "border-2", "border-gray-600", "flex", "items-center", "justify-center", "text-xs", "flex-shrink-0"]] $ do
          case episode.artworkUrl of
            Just artworkUrl ->
              Lucid.img_
                [ Lucid.src_ [i|/#{mediaGetUrl}/#{artworkUrl}|],
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
              Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray700]] "Status: "
              case episode.status of
                Episodes.Draft ->
                  Lucid.span_ [class_ $ base ["inline-block", "bg-yellow-100", "text-yellow-800", "px-2", "py-1", "rounded", "text-xs", Tokens.fontBold]] "DRAFT"
                Episodes.Published ->
                  Lucid.span_ [class_ $ base ["inline-block", "bg-green-100", "text-green-800", "px-2", "py-1", "rounded", "text-xs", Tokens.fontBold]] "PUBLISHED"
                Episodes.Deleted ->
                  Lucid.span_ [class_ $ base ["inline-block", "bg-red-100", "text-red-800", "px-2", "py-1", "rounded", "text-xs", Tokens.fontBold]] "DELETED"

            -- Aired/Scheduled date
            Lucid.div_ $ do
              Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray700]] "Scheduled: "
              case episode.scheduledAt of
                Just scheduledAt -> do
                  let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" scheduledAt
                  Lucid.toHtml dateStr
                Nothing -> "Not scheduled"

            -- Published date
            case episode.publishedAt of
              Just publishedAt -> do
                Lucid.div_ $ do
                  Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray700]] "Published: "
                  let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
                  Lucid.toHtml dateStr
              Nothing -> mempty

            -- Duration
            case episode.durationSeconds of
              Just duration -> do
                let hours = duration `div` 3600
                    minutes = (duration `mod` 3600) `div` 60
                Lucid.div_ $ do
                  Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray700]] "Duration: "
                  if hours > 0
                    then Lucid.toHtml (show hours) <> "h " <> Lucid.toHtml (show minutes) <> "min"
                    else Lucid.toHtml (show minutes) <> "min"
              Nothing -> mempty

          -- Episode description
          case episode.description of
            Just desc -> Lucid.p_ [class_ $ base [Tokens.textGray700, "leading-relaxed"]] $ Lucid.toHtml desc
            Nothing -> mempty

    -- Audio player section
    case episode.audioFilePath of
      Just audioPath -> do
        let episodeNum = episode.episodeNumber
            episodeIdVal = episode.id
            audioUrl :: Text
            audioUrl = [i|/#{mediaGetUrl}/#{audioPath}|]
            playerId :: Text
            playerId = [i|dashboard-episode-#{episodeIdVal}|]
            showTitle = showModel.title
            episodeMetadata :: Text
            episodeMetadata = [i|#{showTitle} - Episode #{episodeNum}|]

        Lucid.div_ [class_ $ base ["border-b-2", "border-gray-800", Tokens.p6]] $ do
          Lucid.h2_ [class_ $ base [Tokens.textLg, Tokens.fontBold, Tokens.mb4, "uppercase"]] "Preview Audio"

          Lucid.div_
            [ class_ $ base [Tokens.bgGray100, "border-2", "border-gray-600", Tokens.p6],
              xData_
                [i|{
                playerId: '#{playerId}',
                isPlaying: false,
                audioUrl: '#{audioUrl}',
                title: '#{episodeMetadata}',
                currentTime: 0,
                duration: 0,
                init() {
                  const audio = this.$refs.audio;
                  audio.addEventListener('loadedmetadata', () => {
                    this.duration = audio.duration;
                  });
                  audio.addEventListener('timeupdate', () => {
                    this.currentTime = audio.currentTime;
                  });
                  audio.addEventListener('ended', () => {
                    this.isPlaying = false;
                  });
                },
                toggle() {
                  this.isPlaying ? this.pause() : this.play();
                },
                play() {
                  if (typeof pauseOtherPlayers !== 'undefined') {
                    pauseOtherPlayers(this.playerId);
                  }
                  const audio = this.$refs.audio;
                  if (!audio.src) audio.src = this.audioUrl;
                  audio.play().then(() => { this.isPlaying = true; });
                },
                pause() {
                  const audio = this.$refs.audio;
                  audio.pause();
                  this.isPlaying = false;
                },
                formatTime(seconds) {
                  if (!seconds || isNaN(seconds)) return '0:00';
                  const hours = Math.floor(seconds / 3600);
                  const mins = Math.floor((seconds % 3600) / 60);
                  const secs = Math.floor(seconds % 60);
                  if (hours > 0) {
                    return hours + ':' + (mins < 10 ? '0' : '') + mins + ':' + (secs < 10 ? '0' : '') + secs;
                  }
                  return mins + ':' + (secs < 10 ? '0' : '') + secs;
                },
                get progress() {
                  if (!this.duration) return 0;
                  return (this.currentTime / this.duration) * 100;
                }
              }|]
            ]
            $ do
              Lucid.audio_ [xRef_ "audio", Lucid.preload_ "metadata"] mempty

              -- Play/pause button and progress bar
              Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4, Tokens.mb4]] $ do
                Lucid.button_
                  [ class_ $ base [Tokens.bgGray800, Tokens.textWhite, "px-8", "py-3", Tokens.fontBold, "hover:bg-gray-700", Tokens.textLg],
                    xOnClick_ "toggle()",
                    xText_ "isPlaying ? 'PAUSE' : 'PLAY'"
                  ]
                  "PLAY"

                Lucid.div_ [Lucid.class_ "flex-grow"] $ do
                  Lucid.div_ [class_ $ base ["bg-gray-300", "h-3", "rounded", "relative"]] $ do
                    Lucid.div_
                      [ class_ $ base [Tokens.bgGray800, "h-3", "rounded", "absolute", "top-0", "left-0"],
                        Lucid.style_ "",
                        xBindStyle_ "{ width: progress + '%' }"
                      ]
                      mempty

                Lucid.span_
                  [ class_ $ base [Tokens.textSm, "font-mono"],
                    xText_ "formatTime(currentTime) + ' / ' + formatTime(duration)"
                  ]
                  "0:00 / 0:00"
      Nothing -> mempty

    -- Track listing section
    unless (null tracks) $ do
      Lucid.div_ [class_ $ base [Tokens.p6]] $ do
        Lucid.h2_ [class_ $ base [Tokens.textLg, Tokens.fontBold, Tokens.mb4, "uppercase", "border-b", "border-gray-800", "pb-2"]] $
          "Track Listing (" <> Lucid.toHtml (show (length tracks)) <> " tracks)"

        Lucid.div_ [Lucid.class_ "space-y-1"] $ do
          mapM_ renderTrackRow tracks

--------------------------------------------------------------------------------

renderTrackRow :: EpisodeTrack.Model -> Lucid.Html ()
renderTrackRow track = do
  Lucid.div_ [class_ $ base ["flex", "justify-between", "items-start", "p-3", "hover:bg-gray-50", "border-b", "border-gray-200"]] $ do
    -- Track number
    Lucid.div_ [class_ $ base ["w-8", "flex-shrink-0", Tokens.textGray600, "font-mono", Tokens.textSm]] $
      Lucid.toHtml (show track.trackNumber <> ".")

    -- Track info
    Lucid.div_ [Lucid.class_ "flex-grow"] $ do
      Lucid.div_ [Lucid.class_ "font-medium"] $ do
        "\"" <> Lucid.toHtml track.title <> "\""

      Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.textGray700]] $ do
        Lucid.toHtml track.artist

--------------------------------------------------------------------------------

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "p-8", "text-center"]] $ do
    Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4]] "Error Loading Episode"
    Lucid.p_ [class_ $ base [Tokens.textGray700, "mb-6"]] $ Lucid.toHtml errorMsg

notFoundTemplate :: Slug -> Lucid.Html ()
notFoundTemplate showSlug = do
  let backUrl = Links.linkURI $ dashboardEpisodesLinks.list showSlug
  Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "p-8", "text-center"]] $ do
    Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4]] "Episode Not Found"
    Lucid.p_ [class_ $ base [Tokens.textGray700, "mb-6"]] "We couldn't find the episode you're looking for."
    Lucid.a_
      [ Lucid.href_ [i|/#{backUrl}|],
        hxGet_ [i|/#{backUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base ["inline-block", Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700"]
      ]
      "Back to Episodes"
