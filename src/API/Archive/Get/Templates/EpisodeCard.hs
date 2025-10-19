module API.Archive.Get.Templates.EpisodeCard (renderEpisodeCard) where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showGetLink)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

renderEpisodeCard :: Episodes.EpisodeWithShow -> Lucid.Html ()
renderEpisodeCard ews = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-4"] $ do
    Lucid.div_ [Lucid.class_ "text-center mb-4"] $ do
      -- Episode artwork
      case ews.ewsArtworkUrl of
        Just artworkUrl ->
          Lucid.div_
            [ Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center mb-4 overflow-hidden"
            ]
            $ Lucid.img_ [Lucid.src_ ("/" <> artworkUrl), Lucid.alt_ ews.ewsTitle, Lucid.class_ "w-full h-full object-cover"]
        Nothing ->
          Lucid.div_
            [ Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center mb-4"
            ]
            $ Lucid.toHtml ("[EPISODE IMG]" :: Text)

      -- Episode title
      Lucid.h3_ [Lucid.class_ "font-bold mb-1"] $ Lucid.toHtml ews.ewsTitle

      -- Show link, host, date
      Lucid.div_ [Lucid.class_ "text-xs text-gray-600 mb-2"] $ do
        let showUrl = "/" <> Text.pack (show (showGetUrl ews.ewsShowSlug))
        Lucid.a_
          [ Lucid.href_ showUrl,
            hxGet_ showUrl,
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "hover:underline"
          ]
          $ Lucid.toHtml ews.ewsShowTitle
        " • "
        Lucid.toHtml ews.ewsHostDisplayName
        " • "
        case ews.ewsPublishedAt of
          Just publishedAt -> Lucid.toHtml $ formatDate publishedAt
          Nothing -> "Draft"

      -- Duration metadata
      Lucid.div_ [Lucid.class_ "text-xs text-gray-600 mb-3"] $ do
        case ews.ewsDurationSeconds of
          Just duration -> Lucid.toHtml $ formatDuration duration
          Nothing -> ""

      -- Genre tags
      case ews.ewsShowGenre of
        Just genre ->
          Lucid.div_ [Lucid.class_ "text-xs mb-3"] $ do
            Lucid.span_ [Lucid.class_ "bg-gray-200 text-gray-800 px-2 py-1 font-mono"] $
              Lucid.toHtml $
                Text.toLower genre
        Nothing -> pure ()

    -- Episode description
    case ews.ewsDescription of
      Just desc ->
        Lucid.p_ [Lucid.class_ "text-sm leading-relaxed mb-4"] $
          Lucid.toHtml $
            truncateText 120 desc
      Nothing -> pure ()

    -- Action buttons
    Lucid.div_ [Lucid.class_ "flex gap-2"] $ do
      case ews.ewsAudioFilePath of
        Just _ ->
          Lucid.button_
            [ Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700 flex-grow"
            ]
            "▶ PLAY"
        Nothing ->
          Lucid.button_
            [ Lucid.class_ "bg-gray-400 text-white px-4 py-2 text-sm font-bold flex-grow cursor-not-allowed",
              Lucid.disabled_ "disabled"
            ]
            "NO AUDIO"
  where
    showGetUrl :: Slug -> Links.URI
    showGetUrl slug = Links.linkURI $ showGetLink slug

formatDate :: UTCTime -> Text
formatDate = Text.pack . formatTime defaultTimeLocale "%b %e, %Y"

formatDuration :: Int64 -> Text
formatDuration totalSeconds =
  let hours = totalSeconds `div` 3600
      minutes = (totalSeconds `mod` 3600) `div` 60
   in if hours > 0
        then Text.pack (show hours) <> "h " <> Text.pack (show minutes) <> "min"
        else Text.pack (show minutes) <> " min"

truncateText :: Int -> Text -> Text
truncateText maxLen text =
  if Text.length text <= maxLen
    then text
    else Text.take maxLen text <> "..."
