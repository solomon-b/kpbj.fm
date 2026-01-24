{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Get.Templates.Episode
  ( renderLatestEpisode,
    renderEpisodeCard,
  )
where

--------------------------------------------------------------------------------

import API.Links (showEpisodesLinks)
import API.Types
import Control.Monad (unless, when)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindStyle_, xData_, xOnClick_, xRef_, xText_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Helper function to get episode detail URL
episodeDetailUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeDetailUrl showSlug epNum = Links.linkURI $ showEpisodesLinks.detail showSlug epNum

-- | Render a featured "Latest Episode" section with full details
renderLatestEpisode :: StorageBackend -> Shows.Model -> Episodes.Model -> [EpisodeTrack.Model] -> Lucid.Html ()
renderLatestEpisode backend showModel episode tracks = do
  Lucid.div_ [class_ $ base [Tokens.bgWhite, "rounded", Tokens.p6, Tokens.mb8]] $ do
    Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4, "uppercase", "border-b", Tokens.borderGray800, Tokens.pb2]] "Latest Episode"

    -- Episode header with image and info
    Lucid.div_ [class_ $ base ["flex", Tokens.gap4, Tokens.mb6]] $ do
      Lucid.div_ [class_ $ base ["w-24", "h-24", "bg-gray-300 dark:bg-gray-600", "border", "border-gray-600 dark:border-gray-500", "flex", "items-center", "justify-center", Tokens.textXs, "flex-shrink-0"]] $ do
        case episode.artworkUrl of
          Just artworkPath -> Lucid.img_ [Lucid.src_ (buildMediaUrl backend artworkPath), Lucid.alt_ "Episode artwork", class_ $ base [Tokens.fullWidth, "h-full", "object-cover"]]
          Nothing -> "[EP IMG]"

      Lucid.div_ [Lucid.class_ "flex-grow"] $ do
        let epNum = episode.episodeNumber
            epUrl = episodeDetailUrl showModel.slug epNum
        Lucid.h3_ [class_ $ base [Tokens.textLg, Tokens.fontBold, Tokens.mb2]] $ do
          Lucid.a_
            [ Lucid.href_ [i|/#{epUrl}|],
              hxGet_ [i|/#{epUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "hover:underline"
            ]
            $ Lucid.toHtml (show epNum)
        Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.textGray600, Tokens.mb2]] $ do
          let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" episode.scheduledAt
          "Aired: " <> Lucid.toHtml dateStr

          case episode.durationSeconds of
            Just duration ->
              let hours = duration `div` 3600
                  minutes = (duration `mod` 3600) `div` 60
               in if hours > 0
                    then " • Duration: " <> Lucid.toHtml (show hours) <> "h " <> Lucid.toHtml (show minutes) <> "min"
                    else " • Duration: " <> Lucid.toHtml (show minutes) <> "min"
            Nothing -> mempty

        case episode.description of
          Just desc -> Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.mb4]] $ Lucid.toHtml desc
          Nothing -> mempty

    -- Audio player (if audio file path exists)
    case episode.audioFilePath of
      Just audioPath -> do
        let showTitle = showModel.title
            episodeNum = episode.episodeNumber
            episodeId = episode.id
            audioUrl :: Text.Text
            audioUrl = buildMediaUrl backend audioPath
            playerId :: Text.Text
            playerId = [i|episode-#{episodeId}|]
            episodeMetadata :: Text.Text
            episodeMetadata = [i|#{showTitle} - Episode #{episodeNum}|]
        Lucid.div_
          [ class_ $ base [Tokens.bgGray100, Tokens.border2, "border-gray-600 dark:border-gray-500", Tokens.p4, Tokens.mb4],
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
            Lucid.audio_ [xRef_ "audio", Lucid.preload_ "none"] mempty
            Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4, Tokens.mb2]] $ do
              Lucid.button_
                [ class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.px6, Tokens.py2, Tokens.fontBold, "hover:bg-gray-700"],
                  xOnClick_ "toggle()",
                  xText_ "isPlaying ? '⏸ PAUSE' : '▶ PLAY'"
                ]
                "▶ PLAY"
              Lucid.div_ [class_ $ base ["flex-grow", "bg-gray-300 dark:bg-gray-600", "h-2", "rounded", "relative"]] $ do
                Lucid.div_
                  [ class_ $ base [Tokens.bgGray800, "h-2", "rounded", "absolute", "top-0", "left-0"],
                    Lucid.style_ "",
                    xBindStyle_ "{ width: progress + '%' }"
                  ]
                  mempty
              Lucid.span_ [class_ $ base [Tokens.textSm, "font-mono"], xText_ "formatTime(currentTime) + ' / ' + formatTime(duration)"] "0:00 / 0:00"
            Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.textGray600]] $ do
              "Now Playing"
      Nothing -> mempty

    -- Track Listing
    unless (null tracks) $ do
      Lucid.div_ [Lucid.class_ Tokens.mb6] $ do
        Lucid.h4_ [class_ $ base [Tokens.fontBold, "mb-3", Tokens.textSm, "uppercase"]] "Track Listing"
        Lucid.div_ [class_ $ base ["space-y-2", Tokens.textSm]] $ do
          mapM_ renderTrack (take 4 tracks)

          when (length tracks > 4) $ do
            Lucid.button_
              [ class_ $ base [Tokens.textXs, Tokens.textGray600, "hover:text-gray-800", "mt-2"],
                Lucid.onclick_ "document.getElementById('all-tracks').classList.toggle('hidden')"
              ]
              $ "+ Show all " <> Lucid.toHtml (show (length tracks)) <> " tracks"
            Lucid.div_ [Lucid.id_ "all-tracks", class_ $ base ["hidden", "space-y-2", "mt-2"]] $ do
              mapM_ renderTrack (drop 4 tracks)
  where
    renderTrack :: EpisodeTrack.Model -> Lucid.Html ()
    renderTrack track = do
      Lucid.div_ [class_ $ base ["flex", "justify-between", Tokens.p2, "hover:bg-gray-50 dark:hover:bg-gray-700"]] $ do
        Lucid.div_ $ do
          Lucid.span_ [Lucid.class_ "font-medium"] $ "\"" <> Lucid.toHtml track.title <> "\""
          " - "
          Lucid.span_ $ Lucid.toHtml track.artist

-- | Render an episode card (for previous episodes list)
renderEpisodeCard :: StorageBackend -> Shows.Model -> Episodes.Model -> Lucid.Html ()
renderEpisodeCard backend showModel episode = do
  Lucid.div_ [class_ $ base ["flex", Tokens.gap4, Tokens.mb6]] $ do
    -- Episode thumbnail
    Lucid.div_ [class_ $ base ["w-24", "h-24", "bg-gray-300 dark:bg-gray-600", "border", "border-gray-600 dark:border-gray-500", "flex", "items-center", "justify-center", Tokens.textXs, "flex-shrink-0"]] $ do
      case episode.artworkUrl of
        Just artworkPath -> Lucid.img_ [Lucid.src_ (buildMediaUrl backend artworkPath), Lucid.alt_ "Episode artwork", class_ $ base [Tokens.fullWidth, "h-full", "object-cover"]]
        Nothing -> "[EP IMG]"

    -- Episode info
    Lucid.div_ [Lucid.class_ "flex-grow"] $ do
      let epNum = episode.episodeNumber
          epUrl = episodeDetailUrl showModel.slug epNum
      Lucid.h3_ [class_ $ base [Tokens.textLg, Tokens.fontBold, Tokens.mb2]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{epUrl}|],
            hxGet_ [i|/#{epUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "hover:underline"
          ]
          $ Lucid.toHtml (show epNum)

      Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.textGray600, Tokens.mb2]] $ do
        let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" episode.scheduledAt
        "Aired: " <> Lucid.toHtml dateStr

        case episode.durationSeconds of
          Just duration -> " • Duration: " <> Lucid.toHtml (show (duration `div` 60)) <> "min"
          Nothing -> mempty

      -- Episode description
      case episode.description of
        Just desc -> do
          let truncatedDesc = Text.take 200 desc
          Lucid.p_ [Lucid.class_ "text-sm mb-4"] $
            Lucid.toHtml $
              truncatedDesc <> if Text.length desc > 200 then "..." else ""
        Nothing -> mempty

      -- Audio player (if audio file path exists)
      case episode.audioFilePath of
        Just audioPath -> do
          let showTitle = showModel.title
              episodeNum = episode.episodeNumber
              episodeId = episode.id
              audioUrl :: Text.Text
              audioUrl = buildMediaUrl backend audioPath
              playerId :: Text.Text
              playerId = [i|episode-#{episodeId}|]
              episodeMetadata :: Text.Text
              episodeMetadata = [i|#{showTitle} - Episode #{episodeNum}|]
          Lucid.div_
            [ Lucid.class_ "bg-gray-100 dark:bg-gray-700 border-2 border-gray-600 dark:border-gray-500 p-4 mb-4",
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
              Lucid.audio_ [xRef_ "audio", Lucid.preload_ "none"] mempty
              Lucid.div_ [Lucid.class_ "flex items-center gap-4 mb-2"] $ do
                Lucid.button_
                  [ Lucid.class_ "bg-gray-800 text-white px-6 py-2 font-bold hover:bg-gray-700",
                    xOnClick_ "toggle()",
                    xText_ "isPlaying ? '⏸ PAUSE' : '▶ PLAY'"
                  ]
                  "▶ PLAY"
                Lucid.div_ [Lucid.class_ "flex-grow bg-gray-300 h-2 rounded relative"] $ do
                  Lucid.div_
                    [ Lucid.class_ "bg-gray-800 h-2 rounded absolute top-0 left-0",
                      Lucid.style_ "",
                      xBindStyle_ "{ width: progress + '%' }"
                    ]
                    mempty
                Lucid.span_ [Lucid.class_ "text-sm font-mono", xText_ "formatTime(currentTime) + ' / ' + formatTime(duration)"] "0:00 / 0:00"
        Nothing -> mempty
