{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Episode.Get.Templates.Page
  ( template,
    errorTemplate,
    notFoundTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, showsLinks)
import API.Types
import Control.Monad (unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design.StyleBuilder.Internal (cls)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindStyle_, xData_, xOnClick_, xRef_, xText_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Helper function to convert paths to full media URL
mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ showsLinks.detail slug Nothing

--------------------------------------------------------------------------------

template :: Shows.Model -> Episodes.Model -> [EpisodeTrack.Model] -> Lucid.Html ()
template showModel episode tracks = do
  Lucid.div_ [Lucid.class_ $ cls ["max-w-4xl", "mx-auto", Tokens.px4, "py-8", Tokens.fullWidth]] $ do
    -- Main episode container
    Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder]] $ do
      -- Episode header with artwork
      Lucid.div_ [Lucid.class_ $ cls ["border-b-2", "border-gray-800", Tokens.p6]] $ do
        Lucid.div_ [Lucid.class_ $ cls ["flex", Tokens.gap6, Tokens.mb6]] $ do
          -- Episode artwork
          Lucid.div_ [Lucid.class_ $ cls ["w-48", "h-48", "bg-gray-300", Tokens.border2, "border-gray-600", "flex", "items-center", "justify-center", Tokens.textXs, "flex-shrink-0"]] $ do
            case episode.artworkUrl of
              Just artworkUrl ->
                Lucid.img_
                  [ Lucid.src_ [i|/#{mediaGetUrl}/#{artworkUrl}|],
                    Lucid.alt_ "Episode artwork",
                    Lucid.class_ "w-full h-full object-cover"
                  ]
              Nothing -> "[EPISODE ARTWORK]"

          -- Episode metadata
          Lucid.div_ [Lucid.class_ "flex-grow"] $ do
            Lucid.div_ [Lucid.class_ $ cls [Tokens.textXs, "uppercase", "tracking-wide", Tokens.textGray600, Tokens.mb2]] $
              "Episode " <> Lucid.toHtml (show episode.episodeNumber)

            Lucid.h1_ [Lucid.class_ $ cls [Tokens.text3xl, Tokens.fontBold, Tokens.mb4]] $ Lucid.toHtml episode.title

            -- Episode info grid
            Lucid.div_ [Lucid.class_ $ cls ["grid", "grid-cols-2", Tokens.gap4, Tokens.textSm, Tokens.mb4]] $ do
              -- Aired date
              Lucid.div_ $ do
                Lucid.span_ [Lucid.class_ $ cls [Tokens.fontBold, Tokens.textGray700]] "Aired: "
                case episode.publishedAt of
                  Just publishedAt -> do
                    let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
                    Lucid.toHtml dateStr
                  Nothing -> case episode.scheduledAt of
                    Just scheduledAt -> do
                      let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" scheduledAt
                      Lucid.toHtml dateStr
                    Nothing -> "Not yet aired"

              -- Duration
              case episode.durationSeconds of
                Just duration -> do
                  let hours = duration `div` 3600
                      minutes = (duration `mod` 3600) `div` 60
                  Lucid.div_ $ do
                    Lucid.span_ [Lucid.class_ $ cls [Tokens.fontBold, Tokens.textGray700]] "Duration: "
                    if hours > 0
                      then Lucid.toHtml (show hours) <> "h " <> Lucid.toHtml (show minutes) <> "min"
                      else Lucid.toHtml (show minutes) <> "min"
                Nothing -> mempty

            -- Episode description
            case episode.description of
              Just desc -> Lucid.p_ [Lucid.class_ $ cls [Tokens.textGray700, "leading-relaxed"]] $ Lucid.toHtml desc
              Nothing -> mempty

      -- Audio player section
      case episode.audioFilePath of
        Just audioPath -> do
          let showTitle = showModel.title
              episodeTitle = episode.title
              episodeNum = episode.episodeNumber
              episodeId = episode.id
              audioUrl :: Text
              audioUrl = [i|/#{mediaGetUrl}/#{audioPath}|]
              playerId :: Text
              playerId = [i|episode-#{episodeId}|]
              episodeMetadata :: Text
              episodeMetadata = [i|#{showTitle} - Episode #{episodeNum}: #{episodeTitle}|]

          Lucid.div_ [Lucid.class_ $ cls ["border-b-2", "border-gray-800", Tokens.p6]] $ do
            Lucid.h2_ [Lucid.class_ $ cls [Tokens.textLg, Tokens.fontBold, Tokens.mb4, "uppercase"]] "Listen Now"

            Lucid.div_
              [ Lucid.class_ $ cls [Tokens.bgGray100, Tokens.border2, "border-gray-600", Tokens.p6],
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
                    pauseOtherPlayers(this.playerId);
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
                Lucid.div_ [Lucid.class_ $ cls ["flex", "items-center", Tokens.gap4, Tokens.mb4]] $ do
                  Lucid.button_
                    [ Lucid.class_ $ cls [Tokens.bgGray800, Tokens.textWhite, "px-8", "py-3", Tokens.fontBold, "hover:bg-gray-700", Tokens.textLg],
                      xOnClick_ "toggle()",
                      xText_ "isPlaying ? '⏸ PAUSE' : '▶ PLAY'"
                    ]
                    "▶ PLAY"

                  Lucid.div_ [Lucid.class_ "flex-grow"] $ do
                    Lucid.div_ [Lucid.class_ $ cls ["bg-gray-300", "h-3", "rounded", "relative"]] $ do
                      Lucid.div_
                        [ Lucid.class_ $ cls [Tokens.bgGray800, "h-3", "rounded", "absolute", "top-0", "left-0"],
                          Lucid.style_ "",
                          xBindStyle_ "{ width: progress + '%' }"
                        ]
                        mempty

                  Lucid.span_
                    [ Lucid.class_ $ cls [Tokens.textSm, "font-mono"],
                      xText_ "formatTime(currentTime) + ' / ' + formatTime(duration)"
                    ]
                    "0:00 / 0:00"
        Nothing -> mempty

      -- Track listing section
      unless (null tracks) $ do
        Lucid.div_ [Lucid.class_ Tokens.p6] $ do
          Lucid.h2_ [Lucid.class_ $ cls [Tokens.textLg, Tokens.fontBold, Tokens.mb4, "uppercase", "border-b", "border-gray-800", Tokens.pb2]] $
            "Track Listing (" <> Lucid.toHtml (show (length tracks)) <> " tracks)"

          Lucid.div_ [Lucid.class_ "space-y-1"] $ do
            mapM_ renderTrackRow tracks

--------------------------------------------------------------------------------

renderTrackRow :: EpisodeTrack.Model -> Lucid.Html ()
renderTrackRow track = do
  Lucid.div_ [Lucid.class_ $ cls ["flex", "justify-between", "items-start", Tokens.p3, "hover:bg-gray-50", "border-b", "border-gray-200"]] $ do
    -- Track number
    Lucid.div_ [Lucid.class_ $ cls ["w-8", "flex-shrink-0", Tokens.textGray600, "font-mono", Tokens.textSm]] $
      Lucid.toHtml (show track.trackNumber <> ".")

    -- Track info
    Lucid.div_ [Lucid.class_ "flex-grow"] $ do
      Lucid.div_ [Lucid.class_ "font-medium"] $ do
        "\"" <> Lucid.toHtml track.title <> "\""

      Lucid.div_ [Lucid.class_ $ cls [Tokens.textSm, Tokens.textGray700]] $ do
        Lucid.toHtml track.artist

--------------------------------------------------------------------------------

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ $ cls ["max-w-2xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    Lucid.div_ [Lucid.class_ $ cls [Tokens.errorBg, Tokens.border2, Tokens.errorBorder, Tokens.p6]] $ do
      Lucid.h1_ [Lucid.class_ $ cls [Tokens.text2xl, Tokens.fontBold, Tokens.mb4, Tokens.errorText]] "Error Loading Episode"
      Lucid.p_ [Lucid.class_ $ cls ["text-red-700", Tokens.mb6]] $ Lucid.toHtml errorMsg
      Lucid.a_
        [ Lucid.href_ "/shows",
          hxGet_ "/shows",
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ $ cls ["inline-block", Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700"]
        ]
        "Back to Shows"

notFoundTemplate :: Slug -> Slug -> Lucid.Html ()
notFoundTemplate showSlug episodeSlug = do
  let showUrl = showGetUrl showSlug
  Lucid.div_ [Lucid.class_ $ cls ["max-w-2xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, "p-8", "text-center"]] $ do
      Lucid.h1_ [Lucid.class_ $ cls [Tokens.text2xl, Tokens.fontBold, Tokens.mb4]] "Episode Not Found"
      Lucid.p_ [Lucid.class_ $ cls [Tokens.textGray700, Tokens.mb6]] $
        "We couldn't find the episode \"" <> Lucid.toHtml (display episodeSlug) <> "\" for show \"" <> Lucid.toHtml (display showSlug) <> "\"."
      Lucid.a_
        [ Lucid.href_ [i|/#{showUrl}|],
          hxGet_ [i|/#{showUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ $ cls ["inline-block", Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700"]
        ]
        "Back to Show"
