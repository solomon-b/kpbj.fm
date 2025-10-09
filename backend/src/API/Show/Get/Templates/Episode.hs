{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Show.Get.Templates.Episode
  ( renderLatestEpisode,
    renderEpisodeCard,
  )
where

--------------------------------------------------------------------------------

import Control.Monad (unless, when)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Lucid qualified

--------------------------------------------------------------------------------

-- | Render a featured "Latest Episode" section with full details
renderLatestEpisode :: Episodes.Model -> [EpisodeTrack.Model] -> Lucid.Html ()
renderLatestEpisode episode tracks = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Latest Episode"

    -- Episode header with image and info
    Lucid.div_ [Lucid.class_ "flex gap-4 mb-6"] $ do
      Lucid.div_ [Lucid.class_ "w-24 h-24 bg-gray-300 border border-gray-600 flex items-center justify-center text-xs flex-shrink-0"] $ do
        case episode.artworkUrl of
          Just artworkUrl -> Lucid.img_ [Lucid.src_ artworkUrl, Lucid.alt_ "Episode artwork", Lucid.class_ "w-full h-full object-cover"]
          Nothing -> "[EP IMG]"

      Lucid.div_ [Lucid.class_ "flex-grow"] $ do
        Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-2"] $ Lucid.toHtml episode.title
        Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-2"] $ do
          case episode.publishedAt of
            Just publishedAt -> do
              let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
              "Aired: " <> Lucid.toHtml dateStr
            Nothing -> "Draft"

          case episode.durationSeconds of
            Just duration ->
              let hours = duration `div` 3600
                  minutes = (duration `mod` 3600) `div` 60
               in if hours > 0
                    then " • Duration: " <> Lucid.toHtml (show hours) <> "h " <> Lucid.toHtml (show minutes) <> "min"
                    else " • Duration: " <> Lucid.toHtml (show minutes) <> "min"
            Nothing -> mempty

        case episode.description of
          Just desc -> Lucid.p_ [Lucid.class_ "text-sm mb-4"] $ Lucid.toHtml desc
          Nothing -> mempty

    -- Audio player (if audio file path exists)
    case episode.audioFilePath of
      Just audioPath -> do
        Lucid.div_ [Lucid.class_ "bg-gray-100 border-2 border-gray-600 p-4 mb-4"] $ do
          Lucid.div_ [Lucid.class_ "flex items-center gap-4 mb-2"] $ do
            Lucid.button_
              [ Lucid.class_ "bg-gray-800 text-white px-6 py-2 font-bold hover:bg-gray-700",
                Lucid.onclick_ [i|playEpisode('#{audioPath}')|]
              ]
              "▶ PLAY"
            Lucid.div_ [Lucid.class_ "flex-grow bg-gray-300 h-2 rounded"] $ do
              Lucid.div_ [Lucid.class_ "bg-gray-800 h-2 rounded w-0"] mempty
            case episode.durationSeconds of
              Just duration ->
                let hours = duration `div` 3600
                    minutes = (duration `mod` 3600) `div` 60
                    seconds = duration `mod` 60
                 in Lucid.span_ [Lucid.class_ "text-sm font-mono"] $
                      "0:00 / "
                        <> (if hours > 0 then Lucid.toHtml (show hours) <> ":" else "")
                        <> Lucid.toHtml (show minutes)
                        <> ":"
                        <> (if seconds < 10 then "0" else "")
                        <> Lucid.toHtml (show seconds)
              Nothing -> mempty
          Lucid.div_ [Lucid.class_ "text-xs text-gray-600"] $ do
            "Now Playing"
      Nothing -> mempty

    -- Track Listing
    unless (null tracks) $ do
      Lucid.div_ [Lucid.class_ "mb-6"] $ do
        Lucid.h4_ [Lucid.class_ "font-bold mb-3 text-sm uppercase"] "Track Listing"
        Lucid.div_ [Lucid.class_ "space-y-2 text-sm"] $ do
          mapM_ renderTrack (take 4 tracks)

          when (length tracks > 4) $ do
            Lucid.button_
              [ Lucid.class_ "text-xs text-gray-600 hover:text-gray-800 mt-2",
                Lucid.onclick_ "document.getElementById('all-tracks').classList.toggle('hidden')"
              ]
              $ "+ Show all " <> Lucid.toHtml (show (length tracks)) <> " tracks"
            Lucid.div_ [Lucid.id_ "all-tracks", Lucid.class_ "hidden space-y-2 mt-2"] $ do
              mapM_ renderTrack (drop 4 tracks)
  where
    renderTrack :: EpisodeTrack.Model -> Lucid.Html ()
    renderTrack track = do
      Lucid.div_ [Lucid.class_ "flex justify-between p-2 hover:bg-gray-50"] $ do
        Lucid.div_ $ do
          Lucid.span_ [Lucid.class_ "font-medium"] $ "\"" <> Lucid.toHtml track.title <> "\""
          " - "
          Lucid.span_ $ Lucid.toHtml track.artist
          case track.album of
            Just album -> Lucid.span_ [Lucid.class_ "text-gray-600 ml-1"] $ " (" <> Lucid.toHtml album <> ")"
            Nothing -> mempty
        case track.duration of
          Just duration -> Lucid.span_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml duration
          Nothing -> mempty

-- | Render an episode card (for previous episodes list)
renderEpisodeCard :: Episodes.Model -> Lucid.Html ()
renderEpisodeCard episode = do
  Lucid.div_ [Lucid.class_ "flex gap-4 mb-6"] $ do
    -- Episode thumbnail
    Lucid.div_ [Lucid.class_ "w-24 h-24 bg-gray-300 border border-gray-600 flex items-center justify-center text-xs flex-shrink-0"] $ do
      case episode.artworkUrl of
        Just artworkUrl -> Lucid.img_ [Lucid.src_ artworkUrl, Lucid.alt_ "Episode artwork", Lucid.class_ "w-full h-full object-cover"]
        Nothing -> "[EP IMG]"

    -- Episode info
    Lucid.div_ [Lucid.class_ "flex-grow"] $ do
      Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-2"] $ Lucid.toHtml episode.title

      Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-2"] $ do
        case episode.publishedAt of
          Just publishedAt -> do
            let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
            "Aired: " <> Lucid.toHtml dateStr
          Nothing -> "Draft"

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
          Lucid.div_ [Lucid.class_ "bg-gray-100 border-2 border-gray-600 p-4 mb-4"] $ do
            Lucid.div_ [Lucid.class_ "flex items-center gap-4 mb-2"] $ do
              Lucid.button_
                [ Lucid.class_ "bg-gray-800 text-white px-6 py-2 font-bold hover:bg-gray-700",
                  Lucid.onclick_ [i|playEpisode('#{audioPath}')|]
                ]
                "▶ PLAY"
              Lucid.div_ [Lucid.class_ "flex-grow bg-gray-300 h-2 rounded"] $ do
                Lucid.div_ [Lucid.class_ "bg-gray-800 h-2 rounded w-0"] mempty
              case episode.durationSeconds of
                Just duration ->
                  let minutes = duration `div` 60
                      seconds = duration `mod` 60
                   in Lucid.span_ [Lucid.class_ "text-sm font-mono"] $
                        "0:00 / "
                          <> Lucid.toHtml (show minutes)
                          <> ":"
                          <> (if seconds < 10 then "0" else "")
                          <> Lucid.toHtml (show seconds)
                Nothing -> mempty
        Nothing -> mempty
